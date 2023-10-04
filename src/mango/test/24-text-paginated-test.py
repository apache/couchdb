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


import mango
import unittest
import time


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class PaginatedResultsTest(mango.DbPerClass):
    # Great enough to make faster systems busy while running the
    # query.
    NUM_DOCS = 10000
    UPDATES = 25

    def setUp(self):
        self.db.recreate()
        self.db.create_text_index(
            analyzer="keyword",
            default_field={},
            selector={},
            fields=[
                {"name": "_id", "type": "string"},
                {"name": "name", "type": "string"},
            ],
            index_array_lengths=True,
        )
        docs = [
            {"_id": f"{doc_id:08X}", "name": mango.random_string(32)}
            for doc_id in range(self.NUM_DOCS)
        ]
        self.db.save_docs(docs)

    def test_query_with_lot_of_results(self):
        # 200 is the maximum for `text` searches.
        docs = self.db.find(selector={"_id": {"$lte": f"{1000:08X}"}}, limit=200)
        assert len(docs) == 200

    def do_query(self, delay, find_args):
        time.sleep(delay)
        return self.db.find(*find_args)

    def do_updates(self, pause, doc_id):
        for i in range(self.UPDATES):
            doc = self.db.open_doc(doc_id)
            updated_doc = {
                "_id": doc_id,
                "_rev": doc["_rev"],
                "update": i,
                "name": "foobar",
            }
            self.db.save_doc(updated_doc)
            time.sleep(pause)

    def test_no_duplicates_on_interleaved_updates(self):
        # Give ~500 ms head start for the updates before running the
        # query.
        query = mango.Concurrently(self.do_query, (0.5, ({"name": "foobar"},)))
        # Keep updating the target document in every 200 ms.
        updates = mango.Concurrently(self.do_updates, (0.2, f"{2:08X}"))
        docs = query.get_result()
        updates.join()
        assert len(docs) == 1

    def test_no_duplicates_on_interleaved_updates_heavy(self):
        query = mango.Concurrently(self.do_query, (0.5, ({"name": "foobar"},)))
        updates = [
            mango.Concurrently(self.do_updates, (0.05, f"{2:08X}")),
            mango.Concurrently(self.do_updates, (0.2, f"{3:08X}")),
            mango.Concurrently(self.do_updates, (0.3, f"{4:08X}")),
            mango.Concurrently(self.do_updates, (0.15, f"{5:08X}")),
            mango.Concurrently(self.do_updates, (0.1, f"{6:08X}")),
        ]
        docs = query.get_result()
        for ref in updates:
            ref.join()
        ids = list(map(lambda d: d["_id"], docs))
        assert sorted(ids) == [
            f"{2:08X}",
            f"{3:08X}",
            f"{4:08X}",
            f"{5:08X}",
            f"{6:08X}",
        ]

    def test_no_duplicates_on_interleaved_updates_with_limit_skip(self):
        query = mango.Concurrently(self.do_query, (0.5, ({"name": "foobar"}, 1, 3)))
        updates = [
            mango.Concurrently(self.do_updates, (0.05, f"{2:08X}")),
            mango.Concurrently(self.do_updates, (0.2, f"{3:08X}")),
            mango.Concurrently(self.do_updates, (0.3, f"{4:08X}")),
            mango.Concurrently(self.do_updates, (0.15, f"{5:08X}")),
            mango.Concurrently(self.do_updates, (0.1, f"{6:08X}")),
        ]
        docs = query.get_result()
        for ref in updates:
            ref.join()
        assert len(docs) == 1
