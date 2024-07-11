% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.
%
% Adapted from https://github.com/GameAnalytics/hyper. Upstream license:
%
% The MIT License (MIT)
% Copyright (c) 2014 Game Analytics ApS
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

% This is a paired down version of [1] with these changes:
%  * Support the binary backend with a few API functions. Remove the NIF and other stuff.
%  * Some of the precisions <11 and >14 are not practical so they are removed.
%  * Apply undercounting fix from [3] (<<Index:P, RegisterValue:(64 - P)/bitstring, _/bitstring>>)
%  * Remove code allowing merging mixed precisios. We don't have that case.
%  * Speed up run_of_zeros/1 function and add a test for it.
%  * Remove unrecheable case in merge_buf/4. See dense_index/2 comment for analysis.
%  * Ensure we have 100% test coverage, including on-disk backward compatibility tests.
%
% [1] https://github.com/GameAnalytics/hyper
% [2] http://static.googleusercontent.com/external_content/untrusted_dlcp/research.google.com/en//pubs/archive/40671.pdf
% [3] https://github.com/LivewareProblems/hyper/commit/b399cc486d5d705dd90f85b31c68e11046e4187c

-module(couch_hyper).

-export([
    new/1,
    insert/2,
    union/1,
    card/1,
    is_hyper/1
]).

% These three records are part of the on-disk format for
% _approx_count_distinct. Consider backwards compatibility when altering them.

-record(buffer, {
    buf,
    buf_size,
    p,
    convert_threshold
}).

-record(dense, {
    b,
    buf,
    buf_size,
    merge_threshold
}).

-type precision() :: 11..14.
-type registers() :: #buffer{} | #dense{}.

-record(hyper, {
    p :: precision(),
    registers :: {hyper_binary, registers()}
}).

-type value() :: binary().
-type filter() :: #hyper{}.

-define(VALUE_SIZE, 6).
-define(MERGE_THRESHOLD, 0.05).

-spec new(precision()) -> filter().
new(P) when is_integer(P), 11 =< P andalso P =< 14 ->
    #hyper{p = P, registers = {hyper_binary, new_buffer(P)}};
new(_) ->
    error(badarg).

-spec is_hyper(filter()) -> boolean().
is_hyper(#hyper{}) ->
    true;
is_hyper(_) ->
    false.

-spec insert(value(), filter()) -> filter().
insert(<<_/binary>> = Value, #hyper{p = P, registers = {hyper_binary, Registers}} = Hyper) ->
    % TODO: switch to xxhash64 which is exactly 64 bits and avoids dealing with sha-1 issues
    Hash = crypto:hash(sha, Value),
    <<Index:P, RegisterValue:(64 - P)/bitstring, _/bitstring>> = Hash,
    ZeroCount = run_of_zeros(RegisterValue) + 1,
    Hyper#hyper{registers = {hyper_binary, set(Index, ZeroCount, Registers)}};
insert(_Value, _Hyper) ->
    error(badarg).

-spec union([filter()]) -> filter().
union([#hyper{p = P, registers = {hyper_binary, _}} = First | _] = Filters) ->
    Registers = lists:map(fun(Filter) -> registers(P, Filter) end, Filters),
    First#hyper{registers = {hyper_binary, max_merge(Registers)}}.

-spec card(filter()) -> float().
card(#hyper{registers = {hyper_binary, Registers0}, p = P}) ->
    M = trunc(pow(2, P)),
    Registers = compact(Registers0),
    RegisterSum = register_sum(Registers),
    E = alpha(M) * pow(M, 2) / RegisterSum,
    Ep =
        case E =< 5 * M of
            true -> E - estimate_bias(E, P);
            false -> E
        end,
    V = zero_count(Registers),
    H =
        case V of
            0 -> Ep;
            _ -> M * math:log(M / V)
        end,
    case H =< threshold(P) of
        true -> round(H);
        false -> round(Ep)
    end.

%% Private functions

registers(Precision, #hyper{p = Precision, registers = {hyper_binary, Registers}}) ->
    Registers.

threshold(11) ->
    1800;
threshold(12) ->
    3100;
threshold(13) ->
    6500;
threshold(14) ->
    11500.

alpha(M) ->
    0.7213 / (1 + 1.079 / M).

pow(X, Y) ->
    math:pow(X, Y).

run_of_zeros(B) ->
    run_of_zeros(B, 0).

run_of_zeros(<<0:1, Rest/bitstring>>, I) ->
    run_of_zeros(Rest, I + 1);
run_of_zeros(_, I) ->
    I.

estimate_bias(E, P) ->
    BiasVector = bias_data(P),
    EstimateVector = estimate_data(P),
    NearestNeighbours = nearest_neighbours(E, EstimateVector),
    lists:sum([element(Index, BiasVector) || Index <- NearestNeighbours]) /
        length(NearestNeighbours).

nearest_neighbours(E, Vector) ->
    Distances = lists:map(
        fun(Index) ->
            V = element(Index, Vector),
            {pow((E - V), 2), Index}
        end,
        lists:seq(1, size(Vector))
    ),
    SortedDistances = lists:keysort(1, Distances),
    {_, Indexes} = lists:unzip(lists:sublist(SortedDistances, 6)),
    Indexes.

new_buffer(P) ->
    M = m(P),
    % 5 words for each entry
    ConvertThreshold = M div (5 * 8),
    #buffer{buf = [], buf_size = 0, p = P, convert_threshold = ConvertThreshold}.

new_dense(P) ->
    M = m(P),
    T = max(trunc(M * ?MERGE_THRESHOLD), 16),
    #dense{b = empty_binary(M), buf = [], buf_size = 0, merge_threshold = T}.

set(Index, Value, #buffer{buf = [{Index, OldValue} | Rest]} = Buffer) ->
    Buffer#buffer{buf = [{Index, max(Value, OldValue)} | Rest]};
set(Index, Value, #buffer{buf = Buf, buf_size = BufSize} = Buffer) ->
    NewBuffer = Buffer#buffer{
        buf = [{Index, Value} | Buf],
        buf_size = BufSize + 1
    },
    case NewBuffer#buffer.buf_size < NewBuffer#buffer.convert_threshold of
        true ->
            NewBuffer;
        false ->
            buffer2dense(NewBuffer)
    end;
set(Index, Value, #dense{buf = Buf, buf_size = BufSize} = Dense) ->
    LeftOffset = Index * ?VALUE_SIZE,
    <<_:LeftOffset/bitstring, R:?VALUE_SIZE/integer, _/bitstring>> = Dense#dense.b,

    if
        R < Value ->
            New = Dense#dense{
                buf = [{Index, Value} | Buf],
                buf_size = BufSize + 1
            },
            case New#dense.buf_size < Dense#dense.merge_threshold of
                true ->
                    New;
                false ->
                    compact(New)
            end;
        true ->
            Dense
    end.

compact(#buffer{} = Buffer) ->
    Buffer;
compact(#dense{b = B, buf = Buf} = Dense) ->
    NewB = merge_buf(B, max_registers(Buf)),
    Dense#dense{b = NewB, buf = [], buf_size = 0}.

max_merge([First | Rest]) ->
    lists:foldl(fun max_merge/2, First, Rest).

max_merge(
    #dense{b = SmallB, buf = SmallBuf, buf_size = SmallBufSize},
    #dense{b = BigB, buf = BigBuf, buf_size = BigBufSize} = Big
) ->
    case SmallBufSize + BigBufSize < Big#dense.merge_threshold of
        true ->
            Merged = do_merge(SmallB, BigB, <<>>),
            Big#dense{b = Merged, buf = SmallBuf ++ BigBuf, buf_size = SmallBufSize + BigBufSize};
        false ->
            BigWithBuf = merge_buf(BigB, max_registers(SmallBuf ++ BigBuf)),
            Merged = do_merge(SmallB, BigWithBuf, <<>>),
            Big#dense{b = Merged, buf = [], buf_size = 0}
    end;
max_merge(
    #buffer{buf = Buf, buf_size = BufferSize},
    #dense{buf = DenseBuf, buf_size = DenseSize} = Dense
) ->
    case BufferSize + DenseSize < Dense#dense.merge_threshold of
        true ->
            Dense#dense{buf = Buf ++ DenseBuf, buf_size = BufferSize + DenseSize};
        false ->
            Merged = max_registers(DenseBuf ++ Buf),
            Dense#dense{buf = Merged, buf_size = length(Merged)}
    end;
max_merge(#dense{} = Dense, #buffer{} = Buffer) ->
    max_merge(Buffer, Dense);
max_merge(
    #buffer{buf = LeftBuf, buf_size = LeftBufSize},
    #buffer{buf = RightBuf, buf_size = RightBufSize} = Right
) ->
    case LeftBufSize + RightBufSize < Right#buffer.convert_threshold of
        true ->
            Right#buffer{buf = LeftBuf ++ RightBuf, buf_size = LeftBufSize + RightBufSize};
        false ->
            Merged = max_registers(LeftBuf ++ RightBuf),
            NewRight = Right#buffer{buf = Merged, buf_size = length(Merged)},
            case NewRight#buffer.buf_size < NewRight#buffer.convert_threshold of
                true -> NewRight;
                false -> buffer2dense(NewRight)
            end
    end.

%% erlfmt-ignore
register_sum(B) ->
    fold(fun (_, 0, Acc) -> Acc + 1.0;
             (_, 1, Acc) -> Acc + 0.5;
             (_, 2, Acc) -> Acc + 0.25;
             (_, 3, Acc) -> Acc + 0.125;
             (_, 4, Acc) -> Acc + 0.0625;
             (_, 5, Acc) -> Acc + 0.03125;
             (_, 6, Acc) -> Acc + 0.015625;
             (_, 7, Acc) -> Acc + 0.0078125;
             (_, 8, Acc) -> Acc + 0.00390625;
             (_, 9, Acc) -> Acc + 0.001953125;
             (_, V, Acc) -> Acc + pow(2, -V) end,
         0, B).

zero_count(B) ->
    Fun = fun
        (_, 0, Acc) -> Acc + 1;
        (_, _, Acc) -> Acc
    end,
    fold(Fun, 0, B).

m(P) ->
    trunc(pow(2, P)).

empty_binary(M) ->
    list_to_bitstring([<<0:?VALUE_SIZE/integer>> || _ <- lists:seq(0, M - 1)]).

max_registers(Buf) ->
    lists:foldl(
        fun({I, V}, Acc) ->
            case orddict:find(I, Acc) of
                {ok, R} when R >= V -> Acc;
                _ -> orddict:store(I, V, Acc)
            end
        end,
        orddict:new(),
        lists:reverse(lists:sort(Buf))
    ).

buffer2dense(#buffer{buf = Buf, p = P}) ->
    Dense = new_dense(P),
    Merged = merge_buf(Dense#dense.b, max_registers(Buf)),
    Dense#dense{b = Merged}.

do_merge(<<>>, <<>>, Acc) ->
    Acc;
do_merge(
    <<Left:?VALUE_SIZE/integer, SmallRest/bitstring>>,
    <<Right:?VALUE_SIZE/integer, BigRest/bitstring>>,
    Acc
) ->
    do_merge(SmallRest, BigRest, <<Acc/bits, (max(Left, Right)):?VALUE_SIZE>>).

fold(F, Acc, #buffer{} = Buffer) ->
    fold(F, Acc, buffer2dense(Buffer));
fold(F, Acc, #dense{b = B, buf = Buf}) ->
    do_fold(F, Acc, merge_buf(B, max_registers(Buf)), 0).

do_fold(_, Acc, <<>>, _) ->
    Acc;
do_fold(F, Acc, <<Value:?VALUE_SIZE/integer, Rest/bitstring>>, Index) ->
    do_fold(F, F(Index, Value, Acc), Rest, Index + 1).

merge_buf(B, L) ->
    merge_buf(B, L, -1, <<>>).

merge_buf(B, [], _PrevIndex, Acc) ->
    <<Acc/bitstring, B/bitstring>>;
merge_buf(B, [{Index, Value} | Rest], PrevIndex, Acc) ->
    I = dense_index(Index, PrevIndex),
    <<Left:I/bitstring, OldValue:?VALUE_SIZE/integer, Right/bitstring>> = B,
    case OldValue < Value of
        true ->
            NewAcc = <<Acc/bitstring, Left/bitstring, Value:?VALUE_SIZE/integer>>,
            merge_buf(Right, Rest, Index, NewAcc);
        false ->
            NewAcc = <<Acc/bitstring, Left/bitstring, OldValue:?VALUE_SIZE/integer>>,
            merge_buf(Right, Rest, Index, NewAcc)
    end.

% Dense index points into a ?VALUE_SIZE (= 6 bit) cell relative to the previous
% index.
%
% For example at P = 11, the total size of the binary block will be (6 bits) *
% 2^11 = 12288 bits. Index values ranges from 0 to 2^11-1 = 2047. The output would
% range from 0 to 12288 - 6 = 12282.
%
% The minimum value is 0 when we step through sequentially, 0 to 1, 1 to 2, etc.:
%    dense_index(0, -1) = 0
%    dense_index(1, 0) = 0
% The maximum value 12282 would be when we start out and have to step to the last
% cell. That points to the start of the last 6 bit cell:
%    dense_index(2047, -1) = 12282
%
dense_index(Index, PrevIndex) ->
    (Index - PrevIndex - 1) * ?VALUE_SIZE.

%% erlfmt-ignore
estimate_data(11) ->
   {
        1477, 1501.6014, 1526.5802, 1551.7942, 1577.3042, 1603.2062,
        1629.8402, 1656.2292, 1682.9462, 1709.9926, 1737.3026, 1765.4252,
        1793.0578, 1821.6092, 1849.626, 1878.5568, 1908.527, 1937.5154,
        1967.1874, 1997.3878, 2027.37, 2058.1972, 2089.5728, 2120.1012,
        2151.9668, 2183.292, 2216.0772, 2247.8578, 2280.6562, 2313.041,
        2345.714, 2380.3112, 2414.1806, 2447.9854, 2481.656, 2516.346,
        2551.5154, 2586.8378, 2621.7448, 2656.6722, 2693.5722, 2729.1462,
        2765.4124, 2802.8728, 2838.898, 2876.408, 2913.4926, 2951.4938,
        2989.6776, 3026.282, 3065.7704, 3104.1012, 3143.7388, 3181.6876,
        3221.1872, 3261.5048, 3300.0214, 3339.806, 3381.409, 3421.4144,
        3461.4294, 3502.2286, 3544.651, 3586.6156, 3627.337, 3670.083,
        3711.1538, 3753.5094, 3797.01, 3838.6686, 3882.1678, 3922.8116,
        3967.9978, 4009.9204, 4054.3286, 4097.5706, 4140.6014, 4185.544,
        4229.5976, 4274.583, 4316.9438, 4361.672, 4406.2786, 4451.8628,
        4496.1834, 4543.505, 4589.1816, 4632.5188, 4678.2294, 4724.8908,
        4769.0194, 4817.052, 4861.4588, 4910.1596, 4956.4344, 5002.5238,
        5048.13, 5093.6374, 5142.8162, 5187.7894, 5237.3984, 5285.6078,
        5331.0858, 5379.1036, 5428.6258, 5474.6018, 5522.7618, 5571.5822,
        5618.59, 5667.9992, 5714.88, 5763.454, 5808.6982, 5860.3644,
        5910.2914, 5953.571, 6005.9232, 6055.1914, 6104.5882, 6154.5702,
        6199.7036, 6251.1764, 6298.7596, 6350.0302, 6398.061, 6448.4694,
        6495.933, 6548.0474, 6597.7166, 6646.9416, 6695.9208, 6742.6328,
        6793.5276, 6842.1934, 6894.2372, 6945.3864, 6996.9228, 7044.2372,
        7094.1374, 7142.2272, 7192.2942, 7238.8338, 7288.9006, 7344.0908,
        7394.8544, 7443.5176, 7490.4148, 7542.9314, 7595.6738, 7641.9878,
        7694.3688, 7743.0448, 7797.522, 7845.53, 7899.594, 7950.3132,
        7996.455, 8050.9442, 8092.9114, 8153.1374, 8197.4472, 8252.8278,
        8301.8728, 8348.6776, 8401.4698, 8453.551, 8504.6598, 8553.8944,
        8604.1276, 8657.6514, 8710.3062, 8758.908, 8807.8706, 8862.1702,
        8910.4668, 8960.77, 9007.2766, 9063.164, 9121.0534, 9164.1354,
        9218.1594, 9267.767, 9319.0594, 9372.155, 9419.7126, 9474.3722,
        9520.1338, 9572.368, 9622.7702, 9675.8448, 9726.5396, 9778.7378,
        9827.6554, 9878.1922, 9928.7782, 9978.3984, 10026.578, 10076.5626,
        10137.1618, 10177.5244, 10229.9176
    };
estimate_data(12) ->
    {
        2954, 3003.4782, 3053.3568, 3104.3666, 3155.324, 3206.9598,
        3259.648, 3312.539, 3366.1474, 3420.2576, 3474.8376, 3530.6076,
        3586.451, 3643.38, 3700.4104, 3757.5638, 3815.9676, 3875.193,
        3934.838, 3994.8548, 4055.018, 4117.1742, 4178.4482, 4241.1294,
        4304.4776, 4367.4044, 4431.8724, 4496.3732, 4561.4304, 4627.5326,
        4693.949, 4761.5532, 4828.7256, 4897.6182, 4965.5186, 5034.4528,
        5104.865, 5174.7164, 5244.6828, 5316.6708, 5387.8312, 5459.9036,
        5532.476, 5604.8652, 5679.6718, 5753.757, 5830.2072, 5905.2828,
        5980.0434, 6056.6264, 6134.3192, 6211.5746, 6290.0816, 6367.1176,
        6447.9796, 6526.5576, 6606.1858, 6686.9144, 6766.1142, 6847.0818,
        6927.9664, 7010.9096, 7091.0816, 7175.3962, 7260.3454, 7344.018,
        7426.4214, 7511.3106, 7596.0686, 7679.8094, 7765.818, 7852.4248,
        7936.834, 8022.363, 8109.5066, 8200.4554, 8288.5832, 8373.366,
        8463.4808, 8549.7682, 8642.0522, 8728.3288, 8820.9528, 8907.727,
        9001.0794, 9091.2522, 9179.988, 9269.852, 9362.6394, 9453.642,
        9546.9024, 9640.6616, 9732.6622, 9824.3254, 9917.7484, 10007.9392,
        10106.7508, 10196.2152, 10289.8114, 10383.5494, 10482.3064,
        10576.8734, 10668.7872, 10764.7156, 10862.0196, 10952.793,
        11049.9748, 11146.0702, 11241.4492, 11339.2772, 11434.2336,
        11530.741, 11627.6136, 11726.311, 11821.5964, 11918.837,
        12015.3724, 12113.0162, 12213.0424, 12306.9804, 12408.4518,
        12504.8968, 12604.586, 12700.9332, 12798.705, 12898.5142,
        12997.0488, 13094.788, 13198.475, 13292.7764, 13392.9698,
        13486.8574, 13590.1616, 13686.5838, 13783.6264, 13887.2638,
        13992.0978, 14081.0844, 14189.9956, 14280.0912, 14382.4956,
        14486.4384, 14588.1082, 14686.2392, 14782.276, 14888.0284,
        14985.1864, 15088.8596, 15187.0998, 15285.027, 15383.6694,
        15495.8266, 15591.3736, 15694.2008, 15790.3246, 15898.4116,
        15997.4522, 16095.5014, 16198.8514, 16291.7492, 16402.6424,
        16499.1266, 16606.2436, 16697.7186, 16796.3946, 16902.3376,
        17005.7672, 17100.814, 17206.8282, 17305.8262, 17416.0744,
        17508.4092, 17617.0178, 17715.4554, 17816.758, 17920.1748,
        18012.9236, 18119.7984, 18223.2248, 18324.2482, 18426.6276,
        18525.0932, 18629.8976, 18733.2588, 18831.0466, 18940.1366,
        19032.2696, 19131.729, 19243.4864, 19349.6932, 19442.866,
        19547.9448, 19653.2798, 19754.4034, 19854.0692, 19965.1224,
        20065.1774, 20158.2212, 20253.353, 20366.3264, 20463.22
    };
estimate_data(13) ->
    {
        5908.5052, 6007.2672, 6107.347, 6208.5794, 6311.2622, 6414.5514,
        6519.3376, 6625.6952, 6732.5988, 6841.3552, 6950.5972, 7061.3082,
        7173.5646, 7287.109, 7401.8216, 7516.4344, 7633.3802, 7751.2962,
        7870.3784, 7990.292, 8110.79, 8233.4574, 8356.6036, 8482.2712,
        8607.7708, 8735.099, 8863.1858, 8993.4746, 9123.8496, 9255.6794,
        9388.5448, 9522.7516, 9657.3106, 9792.6094, 9930.5642, 10068.794,
        10206.7256, 10347.81, 10490.3196, 10632.0778, 10775.9916,
        10920.4662, 11066.124, 11213.073, 11358.0362, 11508.1006,
        11659.1716, 11808.7514, 11959.4884, 12112.1314, 12265.037,
        12420.3756, 12578.933, 12734.311, 12890.0006, 13047.2144,
        13207.3096, 13368.5144, 13528.024, 13689.847, 13852.7528,
        14018.3168, 14180.5372, 14346.9668, 14513.5074, 14677.867,
        14846.2186, 15017.4186, 15184.9716, 15356.339, 15529.2972,
        15697.3578, 15871.8686, 16042.187, 16216.4094, 16389.4188,
        16565.9126, 16742.3272, 16919.0042, 17094.7592, 17273.965,
        17451.8342, 17634.4254, 17810.5984, 17988.9242, 18171.051,
        18354.7938, 18539.466, 18721.0408, 18904.9972, 19081.867,
        19271.9118, 19451.8694, 19637.9816, 19821.2922, 20013.1292,
        20199.3858, 20387.8726, 20572.9514, 20770.7764, 20955.1714,
        21144.751, 21329.9952, 21520.709, 21712.7016, 21906.3868,
        22096.2626, 22286.0524, 22475.051, 22665.5098, 22862.8492,
        23055.5294, 23249.6138, 23437.848, 23636.273, 23826.093,
        24020.3296, 24213.3896, 24411.7392, 24602.9614, 24805.7952,
        24998.1552, 25193.9588, 25389.0166, 25585.8392, 25780.6976,
        25981.2728, 26175.977, 26376.5252, 26570.1964, 26773.387,
        26962.9812, 27163.0586, 27368.164, 27565.0534, 27758.7428,
        27961.1276, 28163.2324, 28362.3816, 28565.7668, 28758.644,
        28956.9768, 29163.4722, 29354.7026, 29561.1186, 29767.9948,
        29959.9986, 30164.0492, 30366.9818, 30562.5338, 30762.9928,
        30976.1592, 31166.274, 31376.722, 31570.3734, 31770.809,
        31974.8934, 32179.5286, 32387.5442, 32582.3504, 32794.076,
        32989.9528, 33191.842, 33392.4684, 33595.659, 33801.8672,
        34000.3414, 34200.0922, 34402.6792, 34610.0638, 34804.0084,
        35011.13, 35218.669, 35418.6634, 35619.0792, 35830.6534,
        36028.4966, 36229.7902, 36438.6422, 36630.7764, 36833.3102,
        37048.6728, 37247.3916, 37453.5904, 37669.3614, 37854.5526,
        38059.305, 38268.0936, 38470.2516, 38674.7064, 38876.167,
        39068.3794, 39281.9144, 39492.8566, 39684.8628, 39898.4108,
        40093.1836, 40297.6858, 40489.7086, 40717.2424
    };
estimate_data(14) ->
    {
        11817.475, 12015.0046, 12215.3792, 12417.7504, 12623.1814,
        12830.0086, 13040.0072, 13252.503, 13466.178, 13683.2738,
        13902.0344, 14123.9798, 14347.394, 14573.7784, 14802.6894,
        15033.6824, 15266.9134, 15502.8624, 15741.4944, 15980.7956,
        16223.8916, 16468.6316, 16715.733, 16965.5726, 17217.204,
        17470.666, 17727.8516, 17986.7886, 18247.6902, 18510.9632,
        18775.304, 19044.7486, 19314.4408, 19587.202, 19862.2576,
        20135.924, 20417.0324, 20697.9788, 20979.6112, 21265.0274,
        21550.723, 21841.6906, 22132.162, 22428.1406, 22722.127,
        23020.5606, 23319.7394, 23620.4014, 23925.2728, 24226.9224,
        24535.581, 24845.505, 25155.9618, 25470.3828, 25785.9702,
        26103.7764, 26420.4132, 26742.0186, 27062.8852, 27388.415,
        27714.6024, 28042.296, 28365.4494, 28701.1526, 29031.8008,
        29364.2156, 29704.497, 30037.1458, 30380.111, 30723.8168,
        31059.5114, 31404.9498, 31751.6752, 32095.2686, 32444.7792,
        32794.767, 33145.204, 33498.4226, 33847.6502, 34209.006,
        34560.849, 34919.4838, 35274.9778, 35635.1322, 35996.3266,
        36359.1394, 36722.8266, 37082.8516, 37447.7354, 37815.9606,
        38191.0692, 38559.4106, 38924.8112, 39294.6726, 39663.973,
        40042.261, 40416.2036, 40779.2036, 41161.6436, 41540.9014,
        41921.1998, 42294.7698, 42678.5264, 43061.3464, 43432.375,
        43818.432, 44198.6598, 44583.0138, 44970.4794, 45353.924,
        45729.858, 46118.2224, 46511.5724, 46900.7386, 47280.6964,
        47668.1472, 48055.6796, 48446.9436, 48838.7146, 49217.7296,
        49613.7796, 50010.7508, 50410.0208, 50793.7886, 51190.2456,
        51583.1882, 51971.0796, 52376.5338, 52763.319, 53165.5534,
        53556.5594, 53948.2702, 54346.352, 54748.7914, 55138.577,
        55543.4824, 55941.1748, 56333.7746, 56745.1552, 57142.7944,
        57545.2236, 57935.9956, 58348.5268, 58737.5474, 59158.5962,
        59542.6896, 59958.8004, 60349.3788, 60755.0212, 61147.6144,
        61548.194, 61946.0696, 62348.6042, 62763.603, 63162.781,
        63560.635, 63974.3482, 64366.4908, 64771.5876, 65176.7346,
        65597.3916, 65995.915, 66394.0384, 66822.9396, 67203.6336,
        67612.2032, 68019.0078, 68420.0388, 68821.22, 69235.8388,
        69640.0724, 70055.155, 70466.357, 70863.4266, 71276.2482,
        71677.0306, 72080.2006, 72493.0214, 72893.5952, 73314.5856,
        73714.9852, 74125.3022, 74521.2122, 74933.6814, 75341.5904,
        75743.0244, 76166.0278, 76572.1322, 76973.1028, 77381.6284,
        77800.6092, 78189.328, 78607.0962, 79012.2508, 79407.8358,
        79825.725, 80238.701, 80646.891, 81035.6436, 81460.0448,
        81876.3884
    }.

%% erlfmt-ignore
bias_data(11) ->
    {
        1476, 1449.6014, 1423.5802, 1397.7942, 1372.3042, 1347.2062,
        1321.8402, 1297.2292, 1272.9462, 1248.9926, 1225.3026, 1201.4252,
        1178.0578, 1155.6092, 1132.626, 1110.5568, 1088.527, 1066.5154,
        1045.1874, 1024.3878, 1003.37, 982.1972, 962.5728, 942.1012,
        922.9668, 903.292, 884.0772, 864.8578, 846.6562, 828.041, 809.714,
        792.3112, 775.1806, 757.9854, 740.656, 724.346, 707.5154,
        691.8378, 675.7448, 659.6722, 645.5722, 630.1462, 614.4124,
        600.8728, 585.898, 572.408, 558.4926, 544.4938, 531.6776, 517.282,
        505.7704, 493.1012, 480.7388, 467.6876, 456.1872, 445.5048,
        433.0214, 420.806, 411.409, 400.4144, 389.4294, 379.2286, 369.651,
        360.6156, 350.337, 342.083, 332.1538, 322.5094, 315.01, 305.6686,
        298.1678, 287.8116, 280.9978, 271.9204, 265.3286, 257.5706,
        249.6014, 242.544, 235.5976, 229.583, 220.9438, 214.672, 208.2786,
        201.8628, 195.1834, 191.505, 186.1816, 178.5188, 172.2294,
        167.8908, 161.0194, 158.052, 151.4588, 148.1596, 143.4344,
        138.5238, 133.13, 127.6374, 124.8162, 118.7894, 117.3984,
        114.6078, 109.0858, 105.1036, 103.6258, 98.6018000000004,
        95.7618000000002, 93.5821999999998, 88.5900000000001,
        86.9992000000002, 82.8800000000001, 80.4539999999997,
        74.6981999999998, 74.3644000000004, 73.2914000000001,
        65.5709999999999, 66.9232000000002, 65.1913999999997,
        62.5882000000001, 61.5702000000001, 55.7035999999998,
        56.1764000000003, 52.7596000000003, 53.0302000000001,
        49.0609999999997, 48.4694, 44.933, 46.0474000000004,
        44.7165999999997, 41.9416000000001, 39.9207999999999,
        35.6328000000003, 35.5276000000003, 33.1934000000001,
        33.2371999999996, 33.3864000000003, 33.9228000000003,
        30.2371999999996, 29.1373999999996, 25.2272000000003,
        24.2942000000003, 19.8338000000003, 18.9005999999999,
        23.0907999999999, 21.8544000000002, 19.5176000000001,
        15.4147999999996, 16.9314000000004, 18.6737999999996,
        12.9877999999999, 14.3688000000002, 12.0447999999997,
        15.5219999999999, 12.5299999999997, 14.5940000000001,
        14.3131999999996, 9.45499999999993, 12.9441999999999,
        3.91139999999996, 13.1373999999996, 5.44720000000052,
        9.82779999999912, 7.87279999999919, 3.67760000000089,
        5.46980000000076, 5.55099999999948, 5.65979999999945,
        3.89439999999922, 3.1275999999998, 5.65140000000065,
        6.3062000000009, 3.90799999999945, 1.87060000000019,
        5.17020000000048, 2.46680000000015, 0.770000000000437,
        -3.72340000000077, 1.16400000000067, 8.05340000000069,
        0.135399999999208, 2.15940000000046, 0.766999999999825,
        1.0594000000001, 3.15500000000065, -0.287399999999252,
        2.37219999999979, -2.86620000000039, -1.63199999999961,
        -2.22979999999916, -0.15519999999924, -1.46039999999994,
        -0.262199999999211, -2.34460000000036, -2.8078000000005,
        -3.22179999999935, -5.60159999999996, -8.42200000000048,
        -9.43740000000071, 0.161799999999857, -10.4755999999998,
        -10.0823999999993
    };
bias_data(12) ->
    {
        2953, 2900.4782, 2848.3568, 2796.3666, 2745.324, 2694.9598,
        2644.648, 2595.539, 2546.1474, 2498.2576, 2450.8376, 2403.6076,
        2357.451, 2311.38, 2266.4104, 2221.5638, 2176.9676, 2134.193,
        2090.838, 2048.8548, 2007.018, 1966.1742, 1925.4482, 1885.1294,
        1846.4776, 1807.4044, 1768.8724, 1731.3732, 1693.4304, 1657.5326,
        1621.949, 1586.5532, 1551.7256, 1517.6182, 1483.5186, 1450.4528,
        1417.865, 1385.7164, 1352.6828, 1322.6708, 1291.8312, 1260.9036,
        1231.476, 1201.8652, 1173.6718, 1145.757, 1119.2072, 1092.2828,
        1065.0434, 1038.6264, 1014.3192, 988.5746, 965.0816, 940.1176,
        917.9796, 894.5576, 871.1858, 849.9144, 827.1142, 805.0818,
        783.9664, 763.9096, 742.0816, 724.3962, 706.3454, 688.018,
        667.4214, 650.3106, 633.0686, 613.8094, 597.818, 581.4248,
        563.834, 547.363, 531.5066, 520.455400000001, 505.583199999999,
        488.366, 476.480799999999, 459.7682, 450.0522, 434.328799999999,
        423.952799999999, 408.727000000001, 399.079400000001,
        387.252200000001, 373.987999999999, 360.852000000001, 351.6394,
        339.642, 330.902400000001, 322.661599999999, 311.662200000001,
        301.3254, 291.7484, 279.939200000001, 276.7508, 263.215200000001,
        254.811400000001, 245.5494, 242.306399999999, 234.8734,
        223.787200000001, 217.7156, 212.0196, 200.793, 195.9748, 189.0702,
        182.449199999999, 177.2772, 170.2336, 164.741, 158.613600000001,
        155.311, 147.5964, 142.837, 137.3724, 132.0162, 130.0424,
        121.9804, 120.451800000001, 114.8968, 111.585999999999,
        105.933199999999, 101.705, 98.5141999999996, 95.0488000000005,
        89.7880000000005, 91.4750000000004, 83.7764000000006,
        80.9698000000008, 72.8574000000008, 73.1615999999995,
        67.5838000000003, 62.6263999999992, 63.2638000000006,
        66.0977999999996, 52.0843999999997, 58.9956000000002,
        47.0912000000008, 46.4956000000002, 48.4383999999991,
        47.1082000000006, 43.2392, 37.2759999999998, 40.0283999999992,
        35.1864000000005, 35.8595999999998, 32.0998, 28.027,
        23.6694000000007, 33.8266000000003, 26.3736000000008,
        27.2008000000005, 21.3245999999999, 26.4115999999995,
        23.4521999999997, 19.5013999999992, 19.8513999999996,
        10.7492000000002, 18.6424000000006, 13.1265999999996,
        18.2436000000016, 6.71860000000015, 3.39459999999963,
        6.33759999999893, 7.76719999999841, 0.813999999998487,
        3.82819999999992, 0.826199999999517, 8.07440000000133,
        -1.59080000000176, 5.01780000000144, 0.455399999998917,
        -0.24199999999837, 0.174800000000687, -9.07640000000174,
        -4.20160000000033, -3.77520000000004, -4.75179999999818,
        -5.3724000000002, -8.90680000000066, -6.10239999999976,
        -5.74120000000039, -9.95339999999851, -3.86339999999836,
        -13.7304000000004, -16.2710000000006, -7.51359999999841,
        -3.30679999999847, -13.1339999999982, -10.0551999999989,
        -6.72019999999975, -8.59660000000076, -10.9307999999983,
        -1.8775999999998, -4.82259999999951, -13.7788, -21.6470000000008,
        -10.6735999999983, -15.7799999999988
    };
bias_data(13) ->
    {
        5907.5052, 5802.2672, 5697.347, 5593.5794, 5491.2622, 5390.5514,
        5290.3376, 5191.6952, 5093.5988, 4997.3552, 4902.5972, 4808.3082,
        4715.5646, 4624.109, 4533.8216, 4444.4344, 4356.3802, 4269.2962,
        4183.3784, 4098.292, 4014.79, 3932.4574, 3850.6036, 3771.2712,
        3691.7708, 3615.099, 3538.1858, 3463.4746, 3388.8496, 3315.6794,
        3244.5448, 3173.7516, 3103.3106, 3033.6094, 2966.5642, 2900.794,
        2833.7256, 2769.81, 2707.3196, 2644.0778, 2583.9916, 2523.4662,
        2464.124, 2406.073, 2347.0362, 2292.1006, 2238.1716, 2182.7514,
        2128.4884, 2077.1314, 2025.037, 1975.3756, 1928.933, 1879.311,
        1831.0006, 1783.2144, 1738.3096, 1694.5144, 1649.024, 1606.847,
        1564.7528, 1525.3168, 1482.5372, 1443.9668, 1406.5074, 1365.867,
        1329.2186, 1295.4186, 1257.9716, 1225.339, 1193.2972, 1156.3578,
        1125.8686, 1091.187, 1061.4094, 1029.4188, 1000.9126, 972.3272,
        944.004199999999, 915.7592, 889.965, 862.834200000001, 840.4254,
        812.598399999999, 785.924200000001, 763.050999999999,
        741.793799999999, 721.466, 699.040799999999, 677.997200000002,
        649.866999999998, 634.911800000002, 609.8694, 591.981599999999,
        570.2922, 557.129199999999, 538.3858, 521.872599999999,
        502.951400000002, 495.776399999999, 475.171399999999, 459.751,
        439.995200000001, 426.708999999999, 413.7016, 402.3868,
        387.262599999998, 372.0524, 357.050999999999, 342.5098,
        334.849200000001, 322.529399999999, 311.613799999999,
        295.848000000002, 289.273000000001, 274.093000000001,
        263.329600000001, 251.389599999999, 245.7392, 231.9614, 229.7952,
        217.155200000001, 208.9588, 199.016599999999, 190.839199999999,
        180.6976, 176.272799999999, 166.976999999999, 162.5252,
        151.196400000001, 149.386999999999, 133.981199999998, 130.0586,
        130.164000000001, 122.053400000001, 110.7428, 108.1276,
        106.232400000001, 100.381600000001, 98.7668000000012,
        86.6440000000002, 79.9768000000004, 82.4722000000002,
        68.7026000000005, 70.1186000000016, 71.9948000000004,
        58.998599999999, 59.0492000000013, 56.9818000000014,
        47.5338000000011, 42.9928, 51.1591999999982, 37.2740000000013,
        42.7220000000016, 31.3734000000004, 26.8090000000011,
        25.8934000000008, 26.5286000000015, 29.5442000000003,
        19.3503999999994, 26.0760000000009, 17.9527999999991,
        14.8419999999969, 10.4683999999979, 8.65899999999965,
        9.86720000000059, 4.34139999999752, -0.907800000000861,
        -3.32080000000133, -0.936199999996461, -11.9916000000012,
        -8.87000000000262, -6.33099999999831, -11.3366000000024,
        -15.9207999999999, -9.34659999999712, -15.5034000000014,
        -19.2097999999969, -15.357799999998, -28.2235999999975,
        -30.6898000000001, -19.3271999999997, -25.6083999999973,
        -24.409599999999, -13.6385999999984, -33.4473999999973,
        -32.6949999999997, -28.9063999999998, -31.7483999999968,
        -32.2935999999972, -35.8329999999987, -47.620600000002,
        -39.0855999999985, -33.1434000000008, -46.1371999999974,
        -37.5892000000022, -46.8164000000033, -47.3142000000007,
        -60.2914000000019, -37.7575999999972
    };
bias_data(14) ->
    {
        11816.475, 11605.0046, 11395.3792, 11188.7504, 10984.1814,
        10782.0086, 10582.0072, 10384.503, 10189.178, 9996.2738,
        9806.0344, 9617.9798, 9431.394, 9248.7784, 9067.6894, 8889.6824,
        8712.9134, 8538.8624, 8368.4944, 8197.7956, 8031.8916, 7866.6316,
        7703.733, 7544.5726, 7386.204, 7230.666, 7077.8516, 6926.7886,
        6778.6902, 6631.9632, 6487.304, 6346.7486, 6206.4408, 6070.202,
        5935.2576, 5799.924, 5671.0324, 5541.9788, 5414.6112, 5290.0274,
        5166.723, 5047.6906, 4929.162, 4815.1406, 4699.127, 4588.5606,
        4477.7394, 4369.4014, 4264.2728, 4155.9224, 4055.581, 3955.505,
        3856.9618, 3761.3828, 3666.9702, 3575.7764, 3482.4132, 3395.0186,
        3305.8852, 3221.415, 3138.6024, 3056.296, 2970.4494, 2896.1526,
        2816.8008, 2740.2156, 2670.497, 2594.1458, 2527.111, 2460.8168,
        2387.5114, 2322.9498, 2260.6752, 2194.2686, 2133.7792, 2074.767,
        2015.204, 1959.4226, 1898.6502, 1850.006, 1792.849, 1741.4838,
        1687.9778, 1638.1322, 1589.3266, 1543.1394, 1496.8266, 1447.8516,
        1402.7354, 1361.9606, 1327.0692, 1285.4106, 1241.8112, 1201.6726,
        1161.973, 1130.261, 1094.2036, 1048.2036, 1020.6436,
        990.901400000002, 961.199800000002, 924.769800000002,
        899.526400000002, 872.346400000002, 834.375, 810.432000000001,
        780.659800000001, 756.013800000001, 733.479399999997,
        707.923999999999, 673.858, 652.222399999999, 636.572399999997,
        615.738599999997, 586.696400000001, 564.147199999999,
        541.679600000003, 523.943599999999, 505.714599999999,
        475.729599999999, 461.779600000002, 449.750800000002,
        439.020799999998, 412.7886, 400.245600000002, 383.188199999997,
        362.079599999997, 357.533799999997, 334.319000000003,
        327.553399999997, 308.559399999998, 291.270199999999,
        279.351999999999, 271.791400000002, 252.576999999997,
        247.482400000001, 236.174800000001, 218.774599999997,
        220.155200000001, 208.794399999999, 201.223599999998,
        182.995600000002, 185.5268, 164.547400000003, 176.5962,
        150.689599999998, 157.8004, 138.378799999999, 134.021200000003,
        117.614399999999, 108.194000000003, 97.0696000000025,
        89.6042000000016, 95.6030000000028, 84.7810000000027,
        72.635000000002, 77.3482000000004, 59.4907999999996,
        55.5875999999989, 50.7346000000034, 61.3916000000027,
        50.9149999999936, 39.0384000000049, 58.9395999999979,
        29.633600000001, 28.2032000000036, 26.0078000000067,
        17.0387999999948, 9.22000000000116, 13.8387999999977,
        8.07240000000456, 14.1549999999988, 15.3570000000036,
        3.42660000000615, 6.24820000000182, -2.96940000000177,
        -8.79940000000352, -5.97860000000219, -14.4048000000039,
        -3.4143999999942, -13.0148000000045, -11.6977999999945,
        -25.7878000000055, -22.3185999999987, -24.409599999999,
        -31.9756000000052, -18.9722000000038, -22.8678000000073,
        -30.8972000000067, -32.3715999999986, -22.3907999999938,
        -43.6720000000059, -35.9038, -39.7492000000057, -54.1641999999993,
        -45.2749999999942, -42.2989999999991, -44.1089999999967,
        -64.3564000000042, -49.9551999999967, -42.6116000000038
     }.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

basics_test() ->
    H = new(11),
    ?assert(is_hyper(H)),
    ?assertNot(is_hyper(potato)),
    ?assertEqual(0, card(H)),
    H1 = insert(<<"x">>, H),
    ?assert(is_hyper(H1)),
    ?assertEqual(1, card(H1)),
    H2 = insert(<<"x">>, H1),
    ?assert(is_hyper(H2)),
    ?assertEqual(1, card(H2)),
    Union = union([H]),
    ?assert(is_hyper(Union)),
    ?assertEqual(0, card(Union)),
    ?assertEqual(0, card(union([H, H]))),
    ?assertEqual(1, card(union([H, H1]))),
    ?assertEqual(1, card(union([H1, H]))),
    ?assertEqual(1, card(union([H1, H1]))),
    ?assertError(badarg, insert(non_binary, H)),
    ?assertError(badarg, insert(<<"y">>, non_hyper)).

valid_precisions_test() ->
    ?assert(is_hyper(new(11))),
    ?assert(is_hyper(new(12))),
    ?assert(is_hyper(new(13))),
    ?assert(is_hyper(new(14))),
    ?assertError(badarg, new(10)),
    ?assertEqual(1, card(insert(<<"x">>, new(11)))),
    ?assertEqual(1, card(insert(<<"x">>, new(12)))),
    ?assertEqual(1, card(insert(<<"x">>, new(13)))),
    ?assertEqual(1, card(insert(<<"x">>, new(14)))).

% Fail if #hyper{}, #buffer{} and #dense{} records are changed
%
on_disk_format_test() ->
    % -record(buffer, {buf, buf_size, p, convert_threshold})
    Buffer = {buffer, [], 0, 11, 1},
    H1 = {hyper, 11, {hyper_binary, Buffer}},
    ?assert(is_hyper(H1)),
    % -record(dense, {b, buf, buf_size, merge_threshold})
    Dense = {dense, empty_binary(m(11)), [], 0, 1},
    H2 = {hyper, 11, {hyper_binary, Dense}},
    ?assert(is_hyper(H2)),
    % Can union current with on-disk formats
    HCur = new(11),
    ?assert(is_hyper(union([H1, HCur]))),
    ?assert(is_hyper(union([H2, HCur]))),
    ?assert(is_hyper(union([HCur, H1]))),
    ?assert(is_hyper(union([HCur, H2]))),
    % Disk formats can union with each other
    ?assert(is_hyper(union([H1, H2]))),
    ?assert(is_hyper(union([H2, H1]))),
    ?assert(is_hyper(union([H1, H1]))),
    ?assert(is_hyper(union([H2, H2]))),
    % Retrieve cardinality existing on-disk formats
    ?assertEqual(0, card(H1)),
    ?assertEqual(0, card(H2)),
    % Update on-disk formats
    ?assert(is_hyper(insert(<<"x">>, H1))),
    ?assert(is_hyper(insert(<<"x">>, H2))).

large_cardinaty_test() ->
    init_rand(),
    H = new(11),
    ?assertEqual(98467, card(insert_rand(100000, H))).

% Updating the buffer with the same value should not make any changes to the
% data structure.
%
same_value_repeated_update_optimization_buffer_test() ->
    HBuff = new(11),
    HBuff1 = insert(<<"x">>, HBuff),
    HBuff2 = insert(<<"x">>, HBuff1),
    ?assertEqual(HBuff2, HBuff1).

% At P = 11, buffer-to-dense threshold is 51, compact threshold is 102.
% Construct various scenarios to achieve decent code coverage in
% max_merge() function, which combines buffered and dense variants.

buffered_under_threshold_union_test() ->
    init_rand(),
    H = new(11),
    ?assertEqual(10, card(union(insert_rand_map([5, 5], H)))).

buffered_over_threshold_union_test() ->
    init_rand(),
    H = new(11),
    ?assertEqual(52, card(union(insert_rand_map([25, 26], H)))).

buffered_over_threshold_with_duplicates_union_test() ->
    % In this case both buffers have duplicates such that after their
    % merge they stay below conversion to dense threshold
    init_rand(),
    H = new(11),
    LeftH = insert_rand(20, H),
    RightH = insert_rand(20, H),
    Items = [integer_to_binary(I) || I <- lists:seq(1, 10)],
    LeftH1 = insert_many(Items, LeftH),
    RightH1 = insert_many(Items, RightH),
    ?assertEqual(51, card(union([LeftH1, RightH1]))).

mixed_under_threshold_union_test() ->
    init_rand(),
    H = new(11),
    ?assertEqual(71, card(union(insert_rand_map([20, 51], H)))).

mixed_over_the_threshold_union_test() ->
    init_rand(),
    H = new(11),
    ?assertEqual(207, card(union(insert_rand_map([50, 150], H)))).

dense_under_threshold_union_test() ->
    init_rand(),
    H = new(11),
    ?assertEqual(104, card(union(insert_rand_map([51, 51], H)))).

dense_over_threshold_union_test() ->
    init_rand(),
    H = new(11),
    ?assertEqual(309, card(union(insert_rand_map([150, 150], H)))).

merge_test() ->
    P = 4,
    M = m(P),
    Tmp1 = [
        {1, 1},
        {3, 3},
        {9, 3},
        {15, 15}
    ],
    Tmp2 = [
        {3, 5},
        {9, 2},
        {10, 5}
    ],

    {buffer, [], 0, T, _} = new_buffer(P),

    {dense, Compact, [], 0, _} =
        compact({dense, empty_binary(M), Tmp1, length(Tmp1), T}),

    {dense, Compact2, [], 0, _} =
        compact({dense, Compact, Tmp2, length(Tmp2), T}),

    Expected = [0, 1, 0, 5, 0, 0, 0, 0, 0, 3, 5, 0, 0, 0, 0, 15],
    Ints = [I || <<I:?VALUE_SIZE/integer>> <= Compact2],

    ?assertEqual(Expected, Ints).

max_registers_test() ->
    ?assertEqual([{3, 3}], max_registers([{3, 1}, {3, 2}, {3, 3}])).

run_of_zeroes_test() ->
    ?assertEqual(0, run_of_zeros(<<>>)),
    ?assertEqual(0, run_of_zeros(<<1:1>>)),
    ?assertEqual(1, run_of_zeros(<<0:1>>)),
    ?assertEqual(1, run_of_zeros(<<0:1, 1:1, 0:1>>)),
    ?assertEqual(1, run_of_zeros(<<0:1, 1:1>>)),
    ?assertEqual(2, run_of_zeros(<<0:2>>)),
    ?assertEqual(2, run_of_zeros(<<0:2, 1:1>>)).

dense_index_test() ->
    % At P = 11, Index max is 2*11 - 1 = 2047
    ?assertEqual(?VALUE_SIZE * 2047, dense_index(2047, -1)),
    ?assertEqual(?VALUE_SIZE * (2047 - 1), dense_index(2047, 0)),
    ?assertEqual(0, dense_index(0, -1)),
    ?assertEqual(0, dense_index(1, 0)),
    ?assertEqual(?VALUE_SIZE, dense_index(1, -1)),
    ?assertEqual(0, dense_index(2047, 2046)).

init_rand() ->
    % This is per-process
    rand:seed(exro928ss, {1, 1, 1}).

insert_rand(0, Hyper) ->
    Hyper;
insert_rand(N, Hyper) when is_integer(N), N > 0 ->
    Hyper1 = insert(rand:bytes(16), Hyper),
    insert_rand(N - 1, Hyper1).

insert_rand_map([_ | _] = Sizes, Hyper) ->
    [insert_rand(Size, Hyper) || Size <- Sizes].

insert_many(Items, Hyper) ->
    Fun = fun(Item, Acc) -> insert(Item, Acc) end,
    lists:foldl(Fun, Hyper, Items).

-endif.
