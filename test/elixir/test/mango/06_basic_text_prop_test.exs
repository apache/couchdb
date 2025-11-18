defmodule BasicTextTest do
     use PropCheck
     use CouchTestCase

    @moduletag :search_props
    @db_name "changeme-proptest-db"
    
    property "test floating point value",
      numtests: 1_000 do # takes about 10 seconds
        MangoDatabase.recreate("/#{@db_name}")
        forall f <- union([float(), "NaN", "Infinity"]) do
            # create doc with float
            :ok == test_float(f)
        end
    end

    # this is used to test the inner logic, as that
    # is a bit tricky from inside the property
    test "single floating point test" do
        MangoDatabase.recreate("/#{@db_name}")
        test_float(3.14)
    end

    def test_float(f) do
        doc = %{"number_string" => f}
        # save doc
        Couch.post("/#{@db_name}", body: doc)
        # run find with same float as $text
        {:ok, docs} = MangoDatabase.find(@db_name, %{"$text" => f})
        # if result length = 1, assert docs[0] == f
        assert_result(docs, f)
        # run find again with same float as field name match
        {:ok, docs} = MangoDatabase.find(@db_name, %{"number_text" => f})
        # repeat same tests
        assert_result(docs, f)
        :ok
    end

    def assert_result(docs, f) do
        cond do
            Enum.count(docs) == 1 ->
                assert Enum.at(docs, 0)["number_string"] == f
            Enum.count(docs) == 2 ->
                if Enum.at(docs, 0)["number_string"] != f do
                    assert Enum.at(docs, 1)["number_string"] == f
                end
            true -> true
        end
    end
end
