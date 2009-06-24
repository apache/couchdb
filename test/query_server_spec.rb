# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License.  You may obtain a copy
# of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
# License for the specific language governing permissions and limitations under
# the License.


# to run (requires ruby and rspec):
# spec test/query_server_spec.rb -f specdoc --color

COUCH_ROOT = "#{File.dirname(__FILE__)}/.." unless defined?(COUCH_ROOT)
LANGUAGE = "js"


require 'open3'
require 'spec'
require 'json'


class OSProcessRunner
  def self.run
    trace = false
    puts "launching #{run_command}" if trace
    if block_given?
      Open3.popen3(run_command) do |jsin, jsout, jserr|
        js = QueryServerRunner.new(jsin, jsout, jserr, trace)
        yield js
      end
    else
      jsin, jsout, jserr = Open3.popen3(run_command)
      QueryServerRunner.new(jsin, jsout, jserr, trace)
    end
  end
  def initialize jsin, jsout, jserr, trace = false
    @qsin = jsin
    @qsout = jsout
    @qserr = jserr
    @trace = trace
  end
  def close
    @qsin.close
    @qsout.close
    @qserr.close
  end
  def reset!
    run(["reset"])
  end
  def add_fun(fun)
    run(["add_fun", fun])
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
    @qsin.puts line
  end
  def rgets
    resp = @qsout.gets
    puts "got: #{resp}"  if @trace
    resp
  end
  def jsgets
    resp = rgets
    # err = @qserr.gets
    # puts "err: #{err}" if err
    if resp
      rj = JSON.parse("[#{resp.chomp}]")[0]
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
  
  COMMANDS = {"js" => "#{COUCH_ROOT}/src/couchdb/couchjs #{COUCH_ROOT}/share/server/main.js" }

  def self.run_command
    COMMANDS[LANGUAGE]
  end
end

class ExternalRunner < OSProcessRunner
  def self.run_command
    "#{COUCH_ROOT}/src/couchdb/couchjs #{COUCH_ROOT}/share/server/echo.js"
  end
end

functions = {
  "emit-twice" => {
    "js" => %{function(doc){emit("foo",doc.a); emit("bar",doc.a)}}
  },
  "emit-once" => {
    "js" => %{function(doc){emit("baz",doc.a)}}
  },
  "reduce-values-length" => {
    "js" => %{function(keys, values, rereduce) { return values.length; }}
  },
  "reduce-values-sum" => {
    "js" => %{function(keys, values, rereduce) { return sum(values); }}
  },
  "validate-forbidden" => {
    "js" => %{function(newDoc, oldDoc, userCtx) { if (newDoc.bad) throw({forbidden:"bad doc"}); "foo bar";}}
  },
  "show-simple" => {
    "js" => <<-JS
        function(doc, req) {
          log("ok");
          return [doc.title, doc.body].join(' - ');
        }
    JS
  },
  "show-headers" => {
    "js" => <<-JS
        function(doc, req) {
          var resp = {"code":200, "headers":{"X-Plankton":"Rusty"}};
          resp.body = [doc.title, doc.body].join(' - ');
          return resp;
        }
     JS
  },
  "show-sends" => {
    "js" =>  <<-JS
        function(head, req) {
          start({headers:{"Content-Type" : "text/plain"}});
          send("first chunk");
          send('second "chunk"');
          return "tail";
        };
    JS
  },
  "show-while-get-rows" => {
    "js" =>  <<-JS
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
  },
  "show-while-get-rows-multi-send" => {
    "js" => <<-JS
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
  },
  "list-simple" => {
    "js" => <<-JS
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
  },
  "list-chunky" => {
    "js" => <<-JS
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
  },
  "list-old-style" => {
    "js" => <<-JS
        function(head, req, foo, bar) {
          return "stuff";
        }
    JS
  },
  "list-capped" => {
    "js" => <<-JS
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
  },
  "list-raw" => {
    "js" => <<-JS
        function(head, req) {
          send("first chunk");
          send(req.q);
          var row;
          while(row = getRow()) {
            send(row.key);        
          };
          return "tail";
        };
    JS
  }
}

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
  
  # it "should validate"
  describe "validation" do
    before(:all) do
      @fun = functions["validate-forbidden"][LANGUAGE]
      @qs.reset!
    end
    it "should allow good updates" do
      @qs.run(["validate", @fun, {"good" => true}, {}, {}]).should == 1
    end
    it "should reject invalid updates" do
      @qs.run(["validate", @fun, {"bad" => true}, {}, {}]).should == {"forbidden"=>"bad doc"}
    end
  end
  
  describe "show" do
    before(:all) do
      @fun = functions["show-simple"][LANGUAGE]
      @qs.reset!
    end
    it "should show" do
      @qs.rrun(["show", @fun, 
        {:title => "Best ever", :body => "Doc body"}])
      @qs.jsgets.should == ["resp", {"body" => "Best ever - Doc body"}]
    end
  end
  
  describe "show with headers" do
    before(:all) do
      @fun = functions["show-headers"][LANGUAGE]
      @qs.reset!
    end
    it "should show headers" do
      @qs.rrun(["show", @fun, 
        {:title => "Best ever", :body => "Doc body"}])
      @qs.jsgets.should == ["resp", {"code"=>200,"headers" => {"X-Plankton"=>"Rusty"}, "body" => "Best ever - Doc body"}]
    end
  end
    
# end
#                    LIST TESTS
# __END__
    
  describe "raw list with headers" do
    before(:each) do
      @fun = functions["show-sends"][LANGUAGE]
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should do headers proper" do
      @qs.rrun(["list", {"total_rows"=>1000}, {"q" => "ok"}])
      @qs.jsgets.should == ["start", ["first chunk", 'second "chunk"'], {"headers"=>{"Content-Type"=>"text/plain"}}]
      @qs.rrun(["list_end"])
      @qs.jsgets.should == ["end", ["tail"]]
    end
  end
  
  describe "list with rows" do
    before(:each) do
      @fun = functions["show-while-get-rows"][LANGUAGE]
      @qs.run(["reset"]).should == true    
      @qs.add_fun(@fun).should == true
    end
    it "should list em" do
      @qs.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @qs.jsgets.should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
      @qs.rrun(["list_row", {"key"=>"baz"}])
      @qs.get_chunks.should == ["baz"]
      @qs.rrun(["list_row", {"key"=>"bam"}])
      @qs.get_chunks.should == ["bam"]
      @qs.rrun(["list_end"])
      @qs.jsgets.should == ["end", ["tail"]]
    end
    it "should work with zero rows" do
      @qs.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @qs.jsgets.should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
      @qs.rrun(["list_end"])
      @qs.jsgets.should == ["end", ["tail"]]
    end
  end
  
  describe "should buffer multiple chunks sent for a single row." do
    before(:all) do
      @fun = functions["show-while-get-rows-multi-send"][LANGUAGE]
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should should buffer em" do
      @qs.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @qs.jsgets.should == ["start", ["bacon"], {"headers"=>{}}]
      @qs.rrun(["list_row", {"key"=>"baz"}])
      @qs.get_chunks.should == ["baz", "eggs"]
      @qs.rrun(["list_row", {"key"=>"bam"}])
      @qs.get_chunks.should == ["bam", "eggs"]
      @qs.rrun(["list_end"])
      @qs.jsgets.should == ["end", ["tail"]]
    end
  end

  describe "example list" do
    before(:all) do
      @fun = functions["list-simple"][LANGUAGE]
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should run normal" do
      @qs.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
      @qs.run(["list_row", {"key"=>"baz"}]).should ==  ["chunks", ["baz"]]
      @qs.run(["list_row", {"key"=>"bam"}]).should ==  ["chunks", ["bam"]]
      @qs.run(["list_row", {"key"=>"foom"}]).should == ["chunks", ["foom"]]
      @qs.run(["list_row", {"key"=>"fooz"}]).should == ["chunks", ["fooz"]]
      @qs.run(["list_row", {"key"=>"foox"}]).should == ["chunks", ["foox"]]
      @qs.run(["list_end"]).should == ["end" , ["early"]]
    end
  end
  
  describe "only goes to 2 list" do
    before(:all) do
      @fun = functions["list-chunky"][LANGUAGE]
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should end early" do
      @qs.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).
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
    "raise before this".should == true
  rescue RuntimeError => e
    e.message.should == "no response"
  rescue Errno::EPIPE
    true.should == true
  end
end

describe "query server that exits" do
  before(:each) do
    @qs = QueryServerRunner.run
  end
  after(:each) do
    @qs.close
  end
  
  describe "old style list" do
    before(:each) do
      @fun = functions["list-old-style"][LANGUAGE]
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should get a warning" do
      resp = @qs.run(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      resp["error"].should == "render_error"
      resp["reason"].should include("the list API has changed")
    end
  end
  
  describe "only goes to 2 list" do
    before(:each) do
      @fun = functions["list-capped"][LANGUAGE]
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should exit if erlang sends too many rows" do
      @qs.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == ["start", ["bacon"], {"headers"=>{}}]
      @qs.run(["list_row", {"key"=>"baz"}]).should ==  ["chunks", ["baz"]]
      @qs.run(["list_row", {"key"=>"foom"}]).should == ["chunks", ["foom"]]
      @qs.run(["list_row", {"key"=>"fooz"}]).should == ["end", ["fooz", "early"]]
      @qs.rrun(["list_row", {"key"=>"foox"}])
      @qs.jsgets["error"].should == "query_server_error"
      should_have_exited @qs
    end
  end
  
  describe "raw list" do
    before(:each) do
      @fun = functions["list-raw"][LANGUAGE]
      @qs.run(["reset"]).should == true
      @qs.add_fun(@fun).should == true
    end
    it "should exit if it gets a non-row in the middle" do
      @qs.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @qs.jsgets.should == ["start", ["first chunk", "ok"], {"headers"=>{}}]
      @qs.run(["reset"])["error"].should == "query_server_error"
      should_have_exited @qs
    end
  end  
end
