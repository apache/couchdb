# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

defmodule Couch.Rate.Limiter.Test do
  use ExUnit.Case, async: true

  @transaction_timeout 5_000

  alias :couch_rate, as: RL

  describe "Stats :" do
    @scenario %{rw_ratio: 1 / 1, target: 400, write_time: 100}
    test "#{__ENV__.line} : #{inspect(@scenario)} (underloaded)" do
      {rate_limiter, measurments} = simulate(@scenario, 1000)
      stats = statistics(measurments)
      maybe_debug(rate_limiter, measurments, stats)

      assert stats.wait_time.p90 == 100,
             "expected no artificial delays for more than 90% of batches"

      budget = stats.budget

      assert floor(budget.p95) in 1..7,
             "expected budget to converge into the 1..7 range (got #{budget.p95})"

      reads = stats.mean_reads

      assert floor(reads.p95) in 1..7,
             "expected mean_read to converge into the 1..7 range (got #{reads.p95})"

      writes = stats.mean_writes
      assert round(writes.p99) in 2..6
      "expected mean_writes to converge into the 2..6 range (got #{writes.p95})"

      assert stats.latency.p95 < @transaction_timeout,
             "expected latency for 95% batches under @transaction_timout"

      found_after = initial_search_speed(measurments)

      assert found_after < 5,
             "expected to find acceptable budget in less than 5 iterations (got: #{
               found_after
             })"

      measurments
      |> initial_search()
      |> Enum.reduce(101, fn row, prev_budget ->
        assert row.budget < prev_budget,
               "expected to reduce budget while we fail"

        row.budget
      end)
    end

    @scenario %{rw_ratio: 1 / 8, target: 3900, write_time: 100}
    test "#{__ENV__.line} : #{inspect(@scenario)} (optimal)" do
      {rate_limiter, measurments} = simulate(@scenario, 1000)
      stats = statistics(measurments)
      maybe_debug(rate_limiter, measurments, stats)

      assert stats.wait_time.p90 == 100,
             "expected no artificial delays for more than 90% of batches"

      budget = stats.budget

      assert floor(budget.p95) in 4..7,
             "expected budget to converge into the 4..7 range (got #{budget.p95})"

      reads = stats.mean_reads

      assert floor(reads.p95) in 4..7,
             "expected mean_read to converge into the 4..7 range (got #{reads.p95})"

      writes = stats.mean_writes
      assert round(writes.p99) in 39..41
      "expected mean_writes to converge into the 39..41 range (got #{writes.p95})"

      assert stats.latency.p95 < @transaction_timeout,
             "expected latency for 95% of batches under @transaction_timout"

      found_after = initial_search_speed(measurments)

      assert found_after < 10,
             "expected to find acceptable budget in less than 10 iterations (got: #{
               found_after
             })"

      measurments
      |> initial_search()
      |> Enum.reduce(101, fn row, prev_budget ->
        assert row.budget < prev_budget,
               "expected to reduce budget while we fail"

        row.budget
      end)
    end

    @scenario %{rw_ratio: 1 / 20, target: 3900, write_time: 100}
    test "#{__ENV__.line} : #{inspect(@scenario)} (overloaded)" do
      # This is a worst case scenario due to big variability of wait_time and
      # big value read/write ratio
      {rate_limiter, measurments} = simulate(@scenario, 1000)
      stats = statistics(measurments)
      maybe_debug(rate_limiter, measurments, stats)

      assert stats.wait_time.p90 == 100,
             "expected no artificial delays for more than 90% of batches"

      budget = stats.budget
      assert floor(budget.p95) in 1..4
      "expected budget to converge into the 1..4 range (got #{budget.p95})"
      reads = stats.mean_reads
      assert floor(reads.p95) in 1..4
      "expected mean_read to converge into the 1..4 range (got #{reads.p95})"
      writes = stats.mean_writes
      assert round(writes.p99) in 39..41
      "expected mean_writes to converge into the 39..41 range (got #{writes.p95})"

      assert stats.latency.p90 < @transaction_timeout,
             "expected latency for 90% of batches under @transaction_timout"

      found_after = initial_search_speed(measurments)

      assert found_after < 16,
             "expected to find acceptable budget in less than 16 iterations (got: #{
               found_after
             })"

      measurments
      |> initial_search()
      |> Enum.reduce(101, fn row, prev_budget ->
        assert row.budget < prev_budget,
               "expected to reduce budget while we fail"

        row.budget
      end)
    end
  end

  defp simulate(scenario, iterations) do
    :couch_rate_ets.create_tables()

    limiter =
      RL.new(:limiter_id, :couch_rate_limiter, nil, %{
        budget: 100,
        target: scenario.target,
        # average over 20 last measurments
        window: scenario.write_time * 20,
        sensitivity: scenario.write_time,
        timer: &timer/0
      })

    result =
      Enum.reduce(0..iterations, {limiter, []}, fn _idx, {limiter, stats} ->
        {budget, limiter} = step(limiter, scenario.rw_ratio, scenario.write_time)
        {limiter, update_measurments(limiter, stats, budget)}
      end)

    :couch_rate_ets.delete_tables()
    result
  end

  defp step(limiter, read_write_ratio, write_time) do
    {reads, limiter} = RL.budget(limiter)
    writes = round(reads / read_write_ratio)
    {delay, limiter} = RL.delay(limiter)
    sleep(delay)
    data_before = RL.to_map(limiter)
    {:ok, limiter} = RL.in(limiter, reads)
    data_after = RL.to_map(limiter)

    assert data_after.size <= data_after.window_size + 1,
           "The number of elements in minimums container shouldn't grow (got: #{
             data_after.size
           })"

    if data_before.writes == 0 and
         data_after.writes == 0 and
         data_before.reads != 0 do
      assert data_before.reads > data_after.reads,
             "expected to reduce number of reads while transaction fails"
    end

    total_write_time =
      0..writes
      |> Enum.reduce_while(0, fn _, acc ->
        write_time = :rand.normal(write_time, write_time * 0.25)

        if acc < @transaction_timeout do
          {:cont, acc + write_time}
        else
          {:halt, acc}
        end
      end)

    sleep(total_write_time)

    if total_write_time < @transaction_timeout do
      {:ok, limiter} = RL.success(limiter, writes)
      {reads, limiter}
    else
      {:ok, limiter} = RL.failure(limiter)
      {reads, limiter}
    end
  end

  defp update_measurments(limiter, stats, budget) do
    data = RL.to_map(limiter)
    {wait_time, _} = RL.delay(limiter)

    stats ++
      [
        %{
          budget: budget,
          slack: data.target - data.latency,
          rw_ratio: data.mean_reads / max(1, data.mean_writes),
          latency: data.latency,
          new_budget: budget,
          minimum_latency: RL.min_latency(limiter),
          wait_time: wait_time,
          elements_in_min_queue: data.size,
          mean_reads: data.mean_reads,
          mean_writes: data.mean_writes,
          total_reads: data.reads,
          total_writes: data.writes
        }
      ]
  end

  defp timer() do
    now = Process.get(:time, 1)
    Process.put(:time, now + 1)
    now
  end

  defp sleep(sleep_time_in_ms) do
    now = timer()
    Process.put(:time, now + sleep_time_in_ms - 1)
  end

  defp format_table([first | _] = rows) do
    spec =
      first
      |> Map.keys()
      |> Enum.map(fn h -> {h, String.length(to_str(h))} end)

    header = first |> Map.keys() |> Enum.map(&to_str/1) |> Enum.join(" , ")

    lines =
      Enum.map(rows, fn row ->
        fields =
          Enum.map(spec, fn {field, size} ->
            String.pad_trailing("#{to_str(Map.get(row, field))}", size)
          end)

        Enum.join(fields, " , ")
      end)

    Enum.join([header | lines], "\n")
  end

  defp initial_search_speed(measurments) do
    length(initial_search(measurments))
  end

  defp initial_search(measurments) do
    Enum.reduce_while(measurments, [], fn row, acc ->
      if row.total_writes == 0 do
        {:cont, acc ++ [row]}
      else
        {:halt, acc}
      end
    end)
  end

  defp statistics(measurments) do
    data =
      Enum.reduce(measurments, %{}, fn row, acc ->
        Enum.reduce(row, acc, fn {key, value}, acc ->
          Map.update(acc, key, [], fn metric ->
            metric ++ [value]
          end)
        end)
      end)

    Enum.reduce(data, %{}, fn {key, values}, acc ->
      stats = Enum.into(:bear.get_statistics(values), %{})
      {percentile, stats} = Map.pop(stats, :percentile)

      stats =
        Enum.reduce(percentile, stats, fn {key, value}, acc ->
          Map.put(acc, String.to_atom("p#{to_str(key)}"), value)
        end)

      Map.put(acc, key, stats)
    end)
  end

  defp format_stats(stats) do
    rows =
      Enum.map(stats, fn {key, values} ->
        values
        |> Enum.into(%{})
        |> Map.put(:metric, key)
        |> Map.delete(:histogram)
      end)

    format_table(rows)
  end

  defp to_str(int) when is_integer(int) do
    "#{int}"
  end

  defp to_str(float) when is_float(float) do
    "#{Float.to_string(Float.round(float, 2))}"
  end

  defp to_str(atom) when is_atom(atom) do
    Atom.to_string(atom)
  end

  defp to_str(string) when is_binary(string) do
    string
  end

  defp to_map(rate_limiter) do
    RL.to_map(rate_limiter)
  end

  defp maybe_debug(rate_limiter, measurments, stats) do
    if System.fetch_env("EXUNIT_DEBUG") != :error do
      IO.puts("")
      IO.puts("rate_limiter: #{inspect(to_map(rate_limiter))}")
      IO.puts("measurments: #{inspect(measurments)}")
      IO.puts("stats: #{inspect(stats)}")

      IO.puts("\n" <> format_table(measurments) <> "\n" <> format_stats(stats))
    end
  end
end
