defmodule PartitionMangoTest do
  use CouchTestCase
  import PartitionHelpers, except: [get_partitions: 1]

  @moduledoc """
  Test Partition functionality for mango
  """
  def create_index(db_name, fields \\ ["some"], opts \\ %{}) do
    default_index = %{
      index: %{
        fields: fields
      }
    }

    index = Enum.into(opts, default_index)
    resp = Couch.post("/#{db_name}/_index", body: index)

    assert resp.status_code == 200
    assert resp.body["result"] == "created"
  end

  def get_partitions(resp) do
    %{:body => %{"docs" => docs}} = resp

    Enum.map(docs, fn doc ->
      [partition, _] = String.split(doc["_id"], ":")
      partition
    end)
  end

  @tag :with_partitioned_db
  test "query using _id and partition works", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name)

    url = "/#{db_name}/_partition/foo/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            _id: %{
              "$gt": "foo:"
            }
          },
          limit: 20
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 20
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            _id: %{
              "$lt": "foo:"
            }
          },
          limit: 20
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 20
    assert_correct_partition(partitions, "bar")
  end

  @tag :with_partitioned_db
  test "query using _id works for global and local query", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name)

    url = "/#{db_name}/_partition/foo/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            _id: %{
              "$gt": 0
            }
          },
          limit: 20
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 20
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            _id: %{
              "$gt": 0
            }
          },
          limit: 20
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 20
    assert_correct_partition(partitions, "bar")
  end

  @tag :with_partitioned_db
  test "query with partitioned:true using index and $eq", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name)

    url = "/#{db_name}/_partition/foo/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            some: "field"
          },
          limit: 20
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 20
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_partition/bar/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            some: "field"
          },
          limit: 20
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 20
    assert_correct_partition(partitions, "bar")
  end

  @tag :with_partitioned_db
  test "partitioned query using _all_docs with $eq", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)

    url = "/#{db_name}/_partition/foo/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            some: "field"
          },
          limit: 20
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 20
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_partition/bar/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            some: "field"
          },
          limit: 20
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 20
    assert_correct_partition(partitions, "bar")
  end

  @tag :with_db
  test "non-partitioned query using _all_docs and $eq", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)

    url = "/#{db_name}/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            some: "field"
          },
          skip: 40,
          limit: 5
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert partitions == ["bar", "bar", "bar", "bar", "bar"]

    url = "/#{db_name}/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            some: "field"
          },
          skip: 50,
          limit: 5
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert partitions == ["foo", "foo", "foo", "foo", "foo"]
  end

  @tag :with_partitioned_db
  test "partitioned query using index and range scan", context do
    db_name = context[:db_name]
    create_partition_docs(db_name, "foo", "bar42")
    create_index(db_name, ["value"])

    url = "/#{db_name}/_partition/foo/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          }
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_partition/bar42/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          }
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert_correct_partition(partitions, "bar42")
  end

  @tag :with_partitioned_db
  test "partitioned query using _all_docs and range scan", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)

    url = "/#{db_name}/_partition/foo/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          }
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_partition/bar/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          }
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert_correct_partition(partitions, "bar")
  end

  @tag :with_partitioned_db
  test "partitioned query using _all_docs", context do
    db_name = context[:db_name]
    create_partition_docs(db_name, "foo", "bar42")

    url = "/#{db_name}/_partition/foo/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          }
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert_correct_partition(partitions, "foo")

    url = "/#{db_name}/_partition/bar42/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          }
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 5
    assert_correct_partition(partitions, "bar42")
  end

  @tag :with_partitioned_db
  test "explain works with partitions", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name, ["some"])

    url = "/#{db_name}/_partition/foo/_explain"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          }
        }
      )

    %{:body => body} = resp

    assert body["index"]["name"] == "_all_docs"
    assert body["mrargs"]["partition"] == "foo"

    url = "/#{db_name}/_partition/bar/_explain"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            some: "field"
          }
        }
      )

    %{:body => body} = resp

    assert body["index"]["def"] == %{"fields" => [%{"some" => "asc"}]}
    assert body["mrargs"]["partition"] == "bar"
  end

  @tag :with_db
  test "explain works with non partitioned db", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name, ["some"])

    url = "/#{db_name}/_explain"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          }
        }
      )

    %{:body => body} = resp

    assert body["index"]["name"] == "_all_docs"
    assert body["mrargs"]["partition"] == :null

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            some: "field"
          }
        }
      )

    %{:body => body} = resp

    assert body["index"]["def"] == %{"fields" => [%{"some" => "asc"}]}
    assert body["mrargs"]["partition"] == :null
  end

  @tag :with_partitioned_db
  test "partitioned query using bookmarks", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name, ["value"])

    url = "/#{db_name}/_partition/foo/_find"

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          },
          limit: 3
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 3
    assert_correct_partition(partitions, "foo")

    %{:body => %{"bookmark" => bookmark}} = resp

    resp =
      Couch.post(url,
        body: %{
          selector: %{
            value: %{
              "$gte": 6,
              "$lt": 16
            }
          },
          limit: 3,
          bookmark: bookmark
        }
      )

    assert resp.status_code == 200
    partitions = get_partitions(resp)
    assert length(partitions) == 2
    assert_correct_partition(partitions, "foo")
  end

  @tag :with_partitioned_db
  test "global query uses global index", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name, ["some"], %{partitioned: false})

    url = "/#{db_name}/_explain"

    selector = %{
      selector: %{
        some: "field"
      },
      limit: 100
    }

    resp = Couch.post(url, body: selector)
    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["index"]["def"] == %{"fields" => [%{"some" => "asc"}]}

    url = "/#{db_name}/_find"
    resp = Couch.post(url, body: selector)
    assert resp.status_code == 200

    partitions = get_partitions(resp)
    assert length(partitions) == 100
  end

  @tag :with_partitioned_db
  test "global query does not use partition index", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name, ["some"])

    url = "/#{db_name}/_explain"

    selector = %{
      selector: %{
        some: "field"
      },
      limit: 100
    }

    resp = Couch.post(url, body: selector)
    %{:body => body} = resp
    assert body["index"]["name"] == "_all_docs"

    url = "/#{db_name}/_find"
    resp = Couch.post(url, body: selector)

    assert resp.status_code == 200

    partitions = get_partitions(resp)
    assert length(partitions) == 100
  end

  @tag :with_partitioned_db
  test "partitioned query does not use global index", context do
    db_name = context[:db_name]
    create_partition_docs(db_name)
    create_index(db_name, ["some"], %{partitioned: false})

    url = "/#{db_name}/_partition/foo/_explain"

    selector = %{
      selector: %{
        some: "field"
      },
      limit: 50
    }

    resp = Couch.post(url, body: selector)
    assert resp.status_code == 200
    %{:body => body} = resp
    assert body["index"]["name"] == "_all_docs"

    url = "/#{db_name}/_partition/foo/_find"
    resp = Couch.post(url, body: selector)
    assert resp.status_code == 200

    partitions = get_partitions(resp)
    assert length(partitions) == 50
    assert_correct_partition(partitions, "foo")
  end
end
