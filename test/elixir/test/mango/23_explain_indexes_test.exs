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

defmodule ExplainIndexJSONTests do
  use CouchTestCase

  @db_name "explain-indexes"

  @candidate_indexes [
    %{
      "index" => %{
        "ddoc" => "_design/user_id",
        "name" => "user_id",
        "type" => "json",
        "partitioned" => false,
        "def" => %{"fields" => [%{"user_id" => "asc"}]},
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      }
    },
    %{
      "index" => %{
        "ddoc" => "_design/twitter",
        "name" => "twitter",
        "type" => "json",
        "partitioned" => false,
        "def" => %{"fields" => [%{"twitter" => "asc"}]},
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      }
    },
    %{
      "index" => %{
        "ddoc" => "_design/ordered",
        "name" => "ordered",
        "type" => "json",
        "partitioned" => false,
        "def" => %{"fields" => [%{"ordered" => "asc"}]},
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      }
    },
    %{
      "index" => %{
        "ddoc" => "_design/name",
        "name" => "name",
        "type" => "json",
        "partitioned" => false,
        "def" => %{"fields" => [%{"name.last" => "asc"}, %{"name.first" => "asc"}]},
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      }
    },
    %{
      "index" => %{
        "ddoc" => "_design/manager",
        "name" => "manager",
        "type" => "json",
        "partitioned" => false,
        "def" => %{"fields" => [%{"manager" => "asc"}]},
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      }
    },
    %{
      "index" => %{
        "ddoc" => "_design/location",
        "name" => "location",
        "type" => "json",
        "partitioned" => false,
        "def" => %{
          "fields" => [
            %{"location.state" => "asc"},
            %{"location.city" => "asc"},
            %{"location.address.street" => "asc"},
            %{"location.address.number" => "asc"},
          ]
        },
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      },
    },
    %{
      "index" => %{
        "ddoc" => "_design/favorites_3",
        "name" => "favorites_3",
        "type" => "json",
        "partitioned" => false,
        "def" => %{"fields" => [%{"favorites.3" => "asc"}]},
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      },
    },
    %{
      "index" => %{
        "ddoc" => "_design/favorites",
        "name" => "favorites",
        "type" => "json",
        "partitioned" => false,
        "def" => %{"fields" => [%{"favorites" => "asc"}]},
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      },
    },
    %{
      "index" => %{
        "ddoc" => "_design/company_and_manager",
        "name" => "company_and_manager",
        "type" => "json",
        "partitioned" => false,
        "def" => %{"fields" => [%{"company" => "asc"}, %{"manager" => "asc"}]},
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 2,
        "covering" => false,
      },
    },
    %{
      "index" => %{
        "ddoc" => nil,
        "name" => "_all_docs",
        "type" => "special",
        "def" => %{"fields" => [%{"_id" => "asc"}]},
      },
      "analysis" => %{
        "usable" => true,
        "reasons" => [%{"name" => "unfavored_type"}],
        "ranking" => 1,
        "covering" => nil,
      }
    }
  ]

  setup do
    UserDocs.setup(@db_name)
  end

  test "basic index candidates" do
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => 23}, explain: true)

    assert explain["index_candidates"] == @candidate_indexes
  end

  test "basic selector hints" do
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => 23}, explain: true)

    selector_hints = Enum.at(explain["selector_hints"], 0)
    assert selector_hints == %{
      "type" => "json",
      "indexable_fields" => ["age"],
      "unindexable_fields" => [],
    }
  end
end

defmodule ExplainIndexTestsNoIndexes do
  use CouchTestCase

  @db_name "explain-indexes-noindex"

  setup do
    UserDocs.setup(@db_name, "special")
  end

  test "basic" do
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => 23}, explain: true)

    assert explain["index_candidates"] == []
    assert explain["index"] != nil
    assert explain["partitioned"] == false
  end

  test "no usable index" do
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => 23}, sort: ["company"], explain: true)

    candidate_indexes = [%{
    "index" => %{
        "ddoc" => nil,
        "name" => "_all_docs",
        "type" => "special",
        "def" => %{"fields" => [%{"_id" => "asc"}]}
      },
      "analysis" => %{
        "usable" => false,
        "reasons" => [%{"name" => "field_mismatch"}],
        "ranking" => 1,
        "covering" => nil,
      }
    }]

    assert explain["index_candidates"] == candidate_indexes
    assert explain["index"] == nil
    assert explain["partitioned"] == false
  end
end

defmodule ExplainIndexTextTests do
  use CouchTestCase

  @db_name "explain-indexes-text"

  setup do
    UserDocs.setup(@db_name, "text")
  end

  test "basic index candidates" do
    candidate_indexes = [
      %{
        "index" => %{
          "ddoc" => nil,
          "name" => "_all_docs",
          "type" => "special",
          "def" => %{"fields" => [%{"_id" => "asc"}]},
        },
        "analysis" => %{
          "usable" => true,
          "reasons" => [%{"name" => "unfavored_type"}],
          "ranking" => 1,
          "covering" => nil,
        }
      }
    ]
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => 23}, explain: true)

    assert explain["index_candidates"] == candidate_indexes
  end

  test "basic selector hints" do
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => 23}, explain: true)

    selector_hints = explain["selector_hints"]
    assert selector_hints == [
      %{"type" => "json", "indexable_fields" => ["age"], "unindexable_fields" => []},
      %{"type" => "text", "indexable_fields" => ["age"], "unindexable_fields" => []},
    ]
  end
end

defmodule ExplainIndexMixedTests do
  use CouchTestCase

  @db_name "explain-indexes-mixed"

  setup do
    UserDocs.setup(@db_name)
    MangoDatabase.create_text_index(@db_name)
    :ok
  end

  test "basic" do
    candidate_indexes = [
      %{
        "index" => %{
          "ddoc" => "_design/user_id",
          "name" => "user_id",
          "type" => "json",
          "partitioned" => false,
          "def" => %{"fields" => [%{"user_id" => "asc"}]},
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/twitter",
          "name" => "twitter",
          "type" => "json",
          "partitioned" => false,
          "def" => %{"fields" => [%{"twitter" => "asc"}]},
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/ordered",
          "name" => "ordered",
          "type" => "json",
          "partitioned" => false,
          "def" => %{"fields" => [%{"ordered" => "asc"}]},
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/name",
          "name" => "name",
          "type" => "json",
          "partitioned" => false,
          "def" => %{"fields" => [%{"name.last" => "asc"}, %{"name.first" => "asc"}]},
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/manager",
          "name" => "manager",
          "type" => "json",
          "partitioned" => false,
          "def" => %{"fields" => [%{"manager" => "asc"}]},
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/location",
          "name" => "location",
          "type" => "json",
          "partitioned" => false,
          "def" => %{
            "fields" => [
              %{"location.state" => "asc"},
              %{"location.city" => "asc"},
              %{"location.address.street" => "asc"},
              %{"location.address.number" => "asc"},
            ]
          },
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/favorites_3",
          "name" => "favorites_3",
          "type" => "json",
          "partitioned" => false,
          "def" => %{"fields" => [%{"favorites.3" => "asc"}]},
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/favorites",
          "name" => "favorites",
          "type" => "json",
          "partitioned" => false,
          "def" => %{"fields" => [%{"favorites" => "asc"}]},
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/company_and_manager",
          "name" => "company_and_manager",
          "type" => "json",
          "partitioned" => false,
          "def" => %{"fields" => [%{"company" => "asc"}, %{"manager" => "asc"}]},
        },
        "analysis" => %{
          "usable" => false,
          "reasons" => [%{"name" => "field_mismatch"}],
          "ranking" => 2,
          "covering" => false,
        },
      },
      %{
        "index" => %{
          "ddoc" => "_design/cedc01a027213706d7260b5e5b73c70b9233743a",
          "name" => "cedc01a027213706d7260b5e5b73c70b9233743a",
          "type" => "text",
          "partitioned" => false,
          "def" => %{
            "default_analyzer" => "keyword",
            "default_field" => %{},
            "selector" => %{},
            "fields" => [],
            "index_array_lengths" => true,
          },
        },
        "analysis" => %{
          "usable" => true,
          "reasons" => [%{"name" => "unfavored_type"}],
          "ranking" => 1,
          "covering" => nil,
        },
      },
      %{
        "index" => %{
          "ddoc" => nil,
          "name" => "_all_docs",
          "type" => "special",
          "def" => %{"fields" => [%{"_id" => "asc"}]},
        },
        "analysis" => %{
          "usable" => true,
          "reasons" => [%{"name" => "unfavored_type"}],
          "ranking" => 1,
          "covering" => nil,
        },
      }]
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => 23}, explain: true)

    assert explain["index_candidates"] == candidate_indexes
  end
end

defmodule ExplainIndexTestsPartitionedNoIndexes do
  use CouchTestCase

  @db_name "explain-indexes-part-noindex"

  setup do
    UserDocs.setup(@db_name, "special", true)
  end

  test "basic" do
    {:ok, explain} = MangoDatabase.find(@db_name, %{"age" => 23}, explain: true)

    assert explain["index_candidates"] == []
    assert explain["index"] != nil
    assert explain["partitioned"] == true
  end
end
