
import mango
import user_docs


class IndexSelectionTests(mango.UserDocsTests):

    def test_basic(self):
        resp = self.db.find({"name.last": "A last name"}, explain=True)
        assert resp["index"]["type"] == "json"

    def test_with_and(self):
        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "This doesn't have to match anything."
            }, explain=True)
        assert resp["index"]["type"] == "json"

    def test_use_most_columns(self):
        # ddoc id for the age index
        ddocid = "_design/ad3d537c03cd7c6a43cf8dff66ef70ea54c2b40f"
        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "Something or other",
                "age": {"$gt": 1}
            }, explain=True)
        assert resp["index"]["ddoc"] != "_design/" + ddocid

        resp = self.db.find({
                "name.first": "Stephanie",
                "name.last": "Something or other",
                "age": {"$gt": 1}
            }, use_index=ddocid, explain=True)
        assert resp["index"]["ddoc"] == ddocid
