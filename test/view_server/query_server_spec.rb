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

# to run (requires ruby and rspec):
# spec test/view_server/query_server_spec.rb -f specdoc --color
# 
# environment options:
#   QS_TRACE=true
#     shows full output from the query server
#   QS_LANG=lang
#     run tests on the query server (for now, one of: js, erlang)
# 

COUCH_ROOT = "#{File.dirname(__FILE__)}/../.." unless defined?(COUCH_ROOT)
LANGUAGE = ENV["QS_LANG"] || "js"

puts "Running query server specs for #{LANGUAGE} query server"

require 'rspec'
require 'json'

class OSProcessRunner
  def self.run
    trace = ENV["QS_TRACE"] || false
    puts "launching #{run_command}" if trace
    if block_given?
      IO.popen(run_command, "r+") do |io|
        qs = QueryServerRunner.new(io, trace)
        yield qs
      end
    else
      io = IO.popen(run_command, "r+")
      QueryServerRunner.new(io, trace)
    end
  end
  def initialize io, trace = false
    @qsio = io
    @trace = trace
  end
  def close
    @qsio.close
  end
  def reset!
    run(["reset"])
  end
  def add_fun(fun)
    run(["add_fun", fun])
  end
  def teach_ddoc(ddoc)
    run(["ddoc", "new", ddoc_id(ddoc), ddoc])
  end
  def ddoc_run(ddoc, fun_path, args)
    run(["ddoc", ddoc_id(ddoc), fun_path, args])
  end
  def ddoc_id(ddoc)
    d_id = ddoc["_id"]
    raise 'ddoc must have _id' unless d_id
    d_id
  end
  def get_chunks
    resp = jsgets
    raise "not a chunk" unless resp.first == "chunks"
    return resp[1]
  end
  def run json
    rrun json
    jsgets
  end
  def rrun json
    line = json.to_json
    puts "run: #{line}" if @trace
    @qsio.puts line
  end
  def rgets
    resp = @qsio.gets
    puts "got: #{resp}"  if @trace
    resp
  end
  def jsgets
    resp = rgets
    # err = @qserr.gets
    # puts "err: #{err}" if err
    if resp
      begin
        rj = JSON.parse("[#{resp.chomp}]")[0]
      rescue JSON::ParserError
        puts "JSON ERROR (dump under trace mode)"
        # puts resp.chomp
        while resp = rgets
          # puts resp.chomp
        end
      end
      if rj.respond_to?(:[]) && rj.is_a?(Array)
        if rj[0] == "log"
          log = rj[1]
          puts "log: #{log}" if @trace
          rj = jsgets
        end
      end
      rj
    else
      raise "no response"
    end
  end
end

class QueryServerRunner < OSProcessRunner

  COMMANDS = {
    "js" => "#{COUCH_ROOT}/bin/couchjs_dev #{COUCH_ROOT}/share/server/main.js",
    "erlang" => "#{COUCH_ROOT}/test/view_server/run_native_process.es"
  }

  def self.run_command
    COMMANDS[LANGUAGE]
  end
end

class ExternalRunner < OSProcessRunner
  def self.run_command
    "#{COUCH_ROOT}/src/couchdb/couchjs #{COUCH_ROOT}/share/server/echo.js"
  end
end

# we could organize this into a design document per language.
# that would make testing future languages really easy.

functions = {
  "emit-twice" => {
    "js" => %{function(doc){emit("foo",doc.a); emit("bar",doc.a)}},
    "erlang" => <<-ERLANG
      fun({Doc}) ->
        A = couch_util:get_value(<<"a">>, Doc, null),
        Emit(<<"foo">>, A),
        Emit(<<"bar">>, A)
      end.
    ERLANG
  },
  "emit-once" => {
    "js" => <<-JS,
      function(doc){
        emit("baz",doc.a)
      }
      JS
    "erlang" => <<-ERLANG
        fun({Doc}) ->
            A = couch_util:get_value(<<"a">>, Doc, null),
            Emit(<<"baz">>, A)
        end.
    ERLANG
  },
  "reduce-values-length" => {
    "js" => %{function(keys, values, rereduce) { return values.length; }},
    "erlang" => %{fun(Keys, Values, ReReduce) -> length(Values) end.}
  },
  "reduce-values-sum" => {
    "js" => %{function(keys, values, rereduce) { return sum(values); }},
    "erlang" => %{fun(Keys, Values, ReReduce) -> lists:sum(Values) end.}
  },
  "validate-forbidden" => {
    "js" => <<-JS,
      function(newDoc, oldDoc, userCtx) {
        if(newDoc.bad)
          throw({forbidden:"bad doc"}); "foo bar";
      }
      JS
    "erlang" => <<-ERLANG
      fun({NewDoc}, _OldDoc, _UserCtx) ->
        case couch_util:get_value(<<"bad">>, NewDoc) of
            undefined -> 1;
            _ -> {[{forbidden, <<"bad doc">>}]}
        end
      end.
    ERLANG
  },
  "show-simple" => {
    "js" => <<-JS,
        function(doc, req) {
            log("ok");
            return [doc.title, doc.body].join(' - ');
        }
    JS
    "erlang" => <<-ERLANG
      fun({Doc}, Req) ->
            Title = couch_util:get_value(<<"title">>, Doc),
            Body = couch_util:get_value(<<"body">>, Doc),
            Resp = <<Title/binary, " - ", Body/binary>>,
        {[{<<"body">>, Resp}]}
      end.
    ERLANG
  },
  "show-headers" => {
    "js" => <<-JS,
        function(doc, req) {
          var resp = {"code":200, "headers":{"X-Plankton":"Rusty"}};
          resp.body = [doc.title, doc.body].join(' - ');
          return resp;
        }
     JS
    "erlang" => <<-ERLANG
  fun({Doc}, Req) ->
        Title = couch_util:get_value(<<"title">>, Doc),
        Body = couch_util:get_value(<<"body">>, Doc),
        Resp = <<Title/binary, " - ", Body/binary>>,
        {[
        {<<"code">>, 200},
        {<<"headers">>, {[{<<"X-Plankton">>, <<"Rusty">>}]}},
        {<<"body">>, Resp}
      ]}
  end.
    ERLANG
  },
  "show-sends" => {
    "js" =>  <<-JS,
        function(head, req) {
          start({headers:{"Content-Type" : "text/plain"}});
          send("first chunk");
          send('second "chunk"');
          return "tail";
        };
    JS
    "erlang" => <<-ERLANG
      fun(Head, Req) ->
        Resp = {[
          {<<"headers">>, {[{<<"Content-Type">>, <<"text/plain">>}]}}
        ]},
        Start(Resp),
        Send(<<"first chunk">>),
        Send(<<"second \\\"chunk\\\"">>),
        <<"tail">>
      end.
    ERLANG
  },
  "show-while-get-rows" => {
    "js" =>  <<-JS,
        function(head, req) {
          send("first chunk");
          send(req.q);
          var row;
          log("about to getRow " + typeof(getRow));
          while(row = getRow()) {
            send(row.key);
          };
          return "tail";
        };
    JS
    "erlang" => <<-ERLANG,
        fun(Head, {Req}) ->
            Send(<<"first chunk">>),
            Send(couch_util:get_value(<<"q">>, Req)),
            Fun = fun({Row}, _) ->
                Send(couch_util:get_value(<<"key">>, Row)),
                {ok, nil}
            end,
            {ok, _} = FoldRows(Fun, nil),
            <<"tail">>
        end.
    ERLANG
  },
  "show-while-get-rows-multi-send" => {
    "js" => <<-JS,
        function(head, req) {
          send("bacon");
          var row;
          log("about to getRow " + typeof(getRow));
          while(row = getRow()) {
            send(row.key);
            send("eggs");
          };
          return "tail";
        };
    JS
    "erlang" => <<-ERLANG,
        fun(Head, Req) ->
            Send(<<"bacon">>),
            Fun = fun({Row}, _) ->
                Send(couch_util:get_value(<<"key">>, Row)),
                Send(<<"eggs">>),
                {ok, nil}
            end,
            FoldRows(Fun, nil),
            <<"tail">>
        end.
    ERLANG
  },
  "list-simple" => {
    "js" => <<-JS,
        function(head, req) {
          send("first chunk");
          send(req.q);
          var row;
          while(row = getRow()) {
            send(row.key);
          };
          return "early";
        };
    JS
    "erlang" => <<-ERLANG,
        fun(Head, {Req}) ->
            Send(<<"first chunk">>),
            Send(couch_util:get_value(<<"q">>, Req)),
            Fun = fun({Row}, _) ->
                Send(couch_util:get_value(<<"key">>, Row)),
                {ok, nil}
            end,
            FoldRows(Fun, nil),
            <<"early">>
        end.
    ERLANG
  },
  "list-chunky" => {
    "js" => <<-JS,
        function(head, req) {
          send("first chunk");
          send(req.q);
          var row, i=0;
          while(row = getRow()) {
            send(row.key);
            i += 1;
            if (i > 2) {
              return('early tail');
            }
          };
        };
    JS
    "erlang" => <<-ERLANG,
        fun(Head, {Req}) ->
            Send(<<"first chunk">>),
            Send(couch_util:get_value(<<"q">>, Req)),
            Fun = fun
                ({Row}, Count) when Count < 2 ->
                    Send(couch_util:get_value(<<"key">>, Row)),
                    {ok, Count+1};
                ({Row}, Count) when Count == 2 ->
                    Send(couch_util:get_value(<<"key">>, Row)),
                    {stop, <<"early tail">>}
            end,
            {ok, Tail} = FoldRows(Fun, 0),
            Tail
        end.
    ERLANG
  },
  "list-old-style" => {
    "js" => <<-JS,
        function(head, req, foo, bar) {
          return "stuff";
        }
    JS
    "erlang" => <<-ERLANG,
        fun(Head, Req, Foo, Bar) ->
            <<"stuff">>
        end.
    ERLANG
  },
  "list-capped" => {
    "js" => <<-JS,
        function(head, req) {
          send("bacon")
          var row, i = 0;
          while(row = getRow()) {
            send(row.key);
            i += 1;
            if (i > 2) {
              return('early');
            }
          };
        }
    JS
    "erlang" => <<-ERLANG,
        fun(Head, Req) ->
            Send(<<"bacon">>),
            Fun = fun
                ({Row}, Count) when Count < 2 ->
                    Send(couch_util:get_value(<<"key">>, Row)),
                    {ok, Count+1};
                ({Row}, Count) when Count == 2 ->
                    Send(couch_util:get_value(<<"key">>, Row)),
                    {stop, <<"early">>}
            end,
            {ok, Tail} = FoldRows(Fun, 0),
            Tail
        end.
    ERLANG
  },
  "list-raw" => {
    "js" => <<-JS,
        function(head, req) {
          // log(this.toSource());
          // log(typeof send);
          send("first chunk");
          send(req.q);
          var row;
          while(row = getRow()) {
            send(row.key);
          };
          return "tail";
        };
    JS
    "erlang" => <<-ERLANG,
        fun(Head, {Req}) ->
            Send(<<"first chunk">>),
            Send(couch_util:get_value(<<"q">>, Req)),
            Fun = fun({Row}, _) ->
                Send(couch_util:get_value(<<"key">>, Row)),
                {ok, nil}
            end,
            FoldRows(Fun, nil),
            <<"tail">>
        end.
    ERLANG
  },
  "filter-basic" => {
    "js" => <<-JS,
      function(doc, req) {
        if (doc.good) {
          return true;
        }
      }
    JS
    "erlang" => <<-ERLANG,
        fun({Doc}, Req) ->
            couch_util:get_value(<<"good">>, Doc)
        end.
    ERLANG
  },
  "update-basic" => {
    "js" => <<-JS,
    function(doc, req) {
      doc.world = "hello";
      var resp = [doc, "hello doc"];
      return resp;
    }
    JS
    "erlang" => <<-ERLANG,
        fun({Doc}, Req) ->
            Doc2 = [{<<"world">>, <<"hello">>}|Doc],
            [{Doc2}, {[{<<"body">>, <<"hello doc">>}]}]
        end.
    ERLANG
  },
  "error" => {
    "js" => <<-JS,
    function() {
      throw(["error","error_key","testing"]);
    }
    JS
    "erlang" => <<-ERLANG
    fun(A, B) ->
      throw([<<"error">>,<<"error_key">>,<<"testing">>])
    end.
    ERLANG
  },
  "fatal" => {
    "js" => <<-JS,
    function() {
      throw(["fatal","error_key","testing"]);
    }
    JS
    "erlang" => <<-ERLANG
    fun(A, B) ->
      throw([<<"fatal">>,<<"error_key">>,<<"testing">>])
    end.
    ERLANG
  }
}

def make_ddoc(fun_path, fun_str)
  doc = {"_id"=>"foo"}
  d = doc
  while p = fun_path.shift
    l = p
    if !fun_path.empty?
      d[p] = {}
      d = d[p]
    end
  end
  d[l] = fun_str
  doc
end

describe "query server normal case" do
  before(:all) do
    `cd #{COUCH_ROOT} && make`
    @qs = QueryServerRunner.run
  end
  after(:all) do
    @qs.close
  end
  it "should reset" do
    @qs.run(["reset"]).should == true
  end
  it "should not erase ddocs on reset" do
    @fun = functions["show-simple"][LANGUAGE]
    @ddoc = make_ddoc(["shows","simple"], @fun)
    @qs.teach_ddoc(@ddoc)
    @qs.run(["reset"]).should == true   
    @qs.ddoc_run(@ddoc, 
      ["shows","simple"], 
      [{:title => "Best ever", :body => "Doc body"}, {}]).should ==
    ["resp", {"body" => "Best ever - Doc body"}] 
  end
  
  it "should run map funs" do
    @qs.reset!
    @qs.run(["add_fun", functions["emit-twice"][LANGUAGE]]).should == true
    @qs.run(["add_fun", functions["emit-once"][LANGUAGE]]).should == true
    rows = @qs.run(["map_doc", {:a => "b"}])
    rows[0][0].should == ["foo", "b"]
    rows[0][1].should == ["bar", "b"]
    rows[1][0].should == ["baz", "b"]
  end
  describe "reduce" do
    before(:all) do
      @fun = functions["reduce-values-length"][LANGUAGE]
      @qs.reset!
    end
    it "should reduce" do
      kvs = (0...10).collect{|i|[i,i*2]}
      @qs.run(["reduce", [@fun], kvs]).should == [true, [10]]
    end
  end
  describe "rereduce" do
    before(:all) do
      @fun = functions["reduce-values-sum"][LANGUAGE]
      @qs.reset!
    end
    it "should rereduce" do
      vs = (0...10).collect{|i|i}
      @qs.run(["rereduce", [@fun], vs]).should == [true, [45]]
    end
  end

  describe "design docs" do
    before(:all) do
      @ddoc = {
        "_id" => "foo"
      }
      @qs.reset!
    end
    it "should learn design docs" do
      @qs.teach_ddoc(@ddoc).should == true
    end
  end

  # it "should validate"
  describe "validation" do
    before(:all) do
      @fun = functions["validate-forbidden"][LANGUAGE]
      @ddoc = make_ddoc(["validate_doc_update"], @fun)
      @qs.teach_ddoc(@ddoc)
    end
    it "should allow good updates" do
      @qs.ddoc_run(@ddoc, 
        ["validate_doc_update"], 
        [{"good" => true}, {}, {}]).should == 1
    end
    it "should reject invalid updates" do
      @qs.ddoc_run(@ddoc, 
        ["validate_doc_update"], 
        [{"bad" => true}, {}, {}]).should == {"forbidden"=>"bad doc"}
    end
  end

  describe "show" do
    before(:all) do
      @fun = functions["show-simple"][LANGUAGE]
      @ddoc = make_ddoc(["shows","simple"], @fun)
      @qs.teach_ddoc(@ddoc)
    end
    it "should show" do
      @qs.ddoc_run(@ddoc, 
        ["shows","simple"], 
        [{:title => "Best ever", :body => "Doc body"}, {}]).should ==
      ["resp", {"body" => "Best ever - Doc body"}]
    end
  end

  describe "show with headers" do
    before(:all) do
      # TODO we can make real ddocs up there. 
      @fun = functions["show-headers"][LANGUAGE]
      @ddoc = make_ddoc(["shows","headers"], @fun)
      @qs.teach_ddoc(@ddoc)
    end
    it "should show headers" do
      @qs.ddoc_run(
        @ddoc, 
        ["shows","headers"], 
        [{:title => "Best ever", :body => "Doc body"}, {}]
      ).
      should == ["resp", {"code"=>200,"headers" => {"X-Plankton"=>"Rusty"}, "body" => "Best ever - Doc body"}]
    end
  end
  
  describe "recoverable error" do
    before(:all) do
      @fun = functions["error"][LANGUAGE]
      @ddoc = make_ddoc(["shows","error"], @fun)
      @qs.teach_ddoc(@ddoc)
    end
    it "should not exit" do
      @qs.ddoc_run(@ddoc, ["shows","error"],
        [{"foo"=>"bar"}, {"q" => "ok"}]).
        should == ["error", "error_key", "testing"]
      # still running
      @qs.run(["reset"]).should == true
    end
  end
  
  describe "changes filter" do
    before(:all) do
      @fun = functions["filter-basic"][LANGUAGE]
      @ddoc = make_ddoc(["filters","basic"], @fun)
      @qs.teach_ddoc(@ddoc)
    end
    it "should only return true for good docs" do
      @qs.ddoc_run(@ddoc, 
        ["filters","basic"], 
        [[{"key"=>"bam", "good" => true}, {"foo" => "bar"}, {"good" => true}], {"req" => "foo"}]
      ).
      should == [true, [true, false, true]]
    end
  end
  
  describe "update" do
    before(:all) do
      # in another patch we can remove this duplication
      # by setting up the design doc for each language ahead of time.
      @fun = functions["update-basic"][LANGUAGE]
      @ddoc = make_ddoc(["updates","basic"], @fun)
      @qs.teach_ddoc(@ddoc)
    end
    it "should return a doc and a resp body" do
      up, doc, resp = @qs.ddoc_run(@ddoc, 
        ["updates","basic"], 
        [{"foo" => "gnarly"}, {"method" => "POST"}]
      )
      up.should == "up"
      doc.should == {"foo" => "gnarly", "world" => "hello"}
      resp["body"].should == "hello doc"
    end
  end

# end
#                    LIST TESTS
# __END__

  describe "ddoc list" do
      before(:all) do
        @ddoc = {
          "_id" => "foo",
          "lists" => {
            "simple" => functions["list-simple"][LANGUAGE],
            "headers" => functions["show-sends"][LANGUAGE],
            "rows" => functions["show-while-get-rows"][LANGUAGE],
            "buffer-chunks" => functions["show-while-get-rows-multi-send"][LANGUAGE],
            "chunky" => functions["list-chunky"][LANGUAGE]
          }
        }
        @qs.teach_ddoc(@ddoc)
      end
      
      describe "example list" do
        it "should run normal" do
          @qs.ddoc_run(@ddoc,
            ["lists","simple"],
            [{"foo"=>"bar"}, {"q" => "ok"}]
          ).should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
          @qs.run(["list_row", {"key"=>"baz"}]).should ==  ["chunks", ["baz"]]
          @qs.run(["list_row", {"key"=>"bam"}]).should ==  ["chunks", ["bam"]]
          @qs.run(["list_row", {"key"=>"foom"}]).should == ["chunks", ["foom"]]
          @qs.run(["list_row", {"key"=>"fooz"}]).should == ["chunks", ["fooz"]]
          @qs.run(["list_row", {"key"=>"foox"}]).should == ["chunks", ["foox"]]
          @qs.run(["list_end"]).should == ["end" , ["early"]]
        end
      end
      
      describe "headers" do
        it "should do headers proper" do
          @qs.ddoc_run(@ddoc, ["lists","headers"], 
            [{"total_rows"=>1000}, {"q" => "ok"}]
          ).should == ["start", ["first chunk", 'second "chunk"'], 
            {"headers"=>{"Content-Type"=>"text/plain"}}]
          @qs.rrun(["list_end"])
          @qs.jsgets.should == ["end", ["tail"]]
        end
      end

      describe "with rows" do
        it "should list em" do
          @qs.ddoc_run(@ddoc, ["lists","rows"], 
            [{"foo"=>"bar"}, {"q" => "ok"}]).
            should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
          @qs.rrun(["list_row", {"key"=>"baz"}])
          @qs.get_chunks.should == ["baz"]
          @qs.rrun(["list_row", {"key"=>"bam"}])
          @qs.get_chunks.should == ["bam"]
          @qs.rrun(["list_end"])
          @qs.jsgets.should == ["end", ["tail"]]
        end
        it "should work with zero rows" do
          @qs.ddoc_run(@ddoc, ["lists","rows"],
            [{"foo"=>"bar"}, {"q" => "ok"}]).
            should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
          @qs.rrun(["list_end"])
          @qs.jsgets.should == ["end", ["tail"]]
        end
      end
      
      describe "should buffer multiple chunks sent for a single row." do
        it "should should buffer em" do
          @qs.ddoc_run(@ddoc, ["lists","buffer-chunks"],
            [{"foo"=>"bar"}, {"q" => "ok"}]).
            should == ["start", ["bacon"], {"headers"=>{}}]
          @qs.rrun(["list_row", {"key"=>"baz"}])
          @qs.get_chunks.should == ["baz", "eggs"]
          @qs.rrun(["list_row", {"key"=>"bam"}])
          @qs.get_chunks.should == ["bam", "eggs"]
          @qs.rrun(["list_end"])
          @qs.jsgets.should == ["end", ["tail"]]
        end
      end
      it "should end after 2" do
        @qs.ddoc_run(@ddoc, ["lists","chunky"],
          [{"foo"=>"bar"}, {"q" => "ok"}]).
          should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
          
        @qs.run(["list_row", {"key"=>"baz"}]).
          should ==  ["chunks", ["baz"]]

        @qs.run(["list_row", {"key"=>"bam"}]).
          should ==  ["chunks", ["bam"]]

        @qs.run(["list_row", {"key"=>"foom"}]).
          should == ["end", ["foom", "early tail"]]
        # here's where js has to discard quit properly
        @qs.run(["reset"]).
          should == true
      end
    end
  end



def should_have_exited qs
  begin
    qs.run(["reset"])
    "raise before this (except Erlang)".should == true
  rescue RuntimeError => e
    e.message.should == "no response"
  rescue Errno::EPIPE
    true.should == true
  end
end

describe "query server that exits" do
  before(:each) do
    @qs = QueryServerRunner.run
    @ddoc = {
      "_id" => "foo",
      "lists" => {
        "capped" => functions["list-capped"][LANGUAGE],
        "raw" => functions["list-raw"][LANGUAGE]
      },
      "shows" => {
        "fatal" => functions["fatal"][LANGUAGE]
      }
    }
    @qs.teach_ddoc(@ddoc)
  end
  after(:each) do
    @qs.close
  end

  describe "only goes to 2 list" do
    it "should exit if erlang sends too many rows" do
      @qs.ddoc_run(@ddoc, ["lists","capped"],
        [{"foo"=>"bar"}, {"q" => "ok"}]).
        should == ["start", ["bacon"], {"headers"=>{}}]
      @qs.run(["list_row", {"key"=>"baz"}]).should ==  ["chunks", ["baz"]]
      @qs.run(["list_row", {"key"=>"foom"}]).should == ["chunks", ["foom"]]
      @qs.run(["list_row", {"key"=>"fooz"}]).should == ["end", ["fooz", "early"]]
      e = @qs.run(["list_row", {"key"=>"foox"}])
      e[0].should == "error"
      e[1].should == "unknown_command"
      should_have_exited @qs
    end
  end

  describe "raw list" do
    it "should exit if it gets a non-row in the middle" do
      @qs.ddoc_run(@ddoc, ["lists","raw"],
        [{"foo"=>"bar"}, {"q" => "ok"}]).
        should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
      e = @qs.run(["reset"])
      e[0].should == "error"
      e[1].should == "list_error"
      should_have_exited @qs
    end
  end
  
  describe "fatal error" do
    it "should exit" do
      @qs.ddoc_run(@ddoc, ["shows","fatal"],
        [{"foo"=>"bar"}, {"q" => "ok"}]).
        should == ["error", "error_key", "testing"]
      should_have_exited @qs
    end
  end
end

describe "thank you for using the tests" do
  it "for more info run with QS_TRACE=true or see query_server_spec.rb file header" do
  end
end
