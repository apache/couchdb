package org.apache.couchdb.nouveau.core;

import io.dropwizard.ConfiguredBundle;

public interface LuceneBundle<T> extends ConfiguredBundle<T> {

    Lucene getLucene();

}
