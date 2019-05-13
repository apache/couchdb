defprotocol Couch.Test.Adapter do
  def login(adapter, user, pass)
  def create_user(adapter, user \\ [])
  def create_user_from_doc(adapter, user_doc)
  def create_db(adapter, db_name, opts \\ [])
  def delete_db(adapter, db_name)
  def create_doc(adapter, db_name, body)
  def open_doc(adapter, db_name, doc_id)
  def update_doc(adapter, db_name, body)
  def delete_doc(adapter, db_name, doc_id, rev)

  def bulk_save(adapter, db_name, docs)
  def query(
        adapter,
        db_name,
        map_fun,
        reduce_fun \\ nil,
        options \\ nil,
        keys \\ nil,
        language \\ "javascript"
      )
  def set_config(adapter, section, key, val)

end
