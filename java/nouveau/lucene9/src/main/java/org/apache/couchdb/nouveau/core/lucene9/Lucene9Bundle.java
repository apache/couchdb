package org.apache.couchdb.nouveau.core.lucene9;

import org.apache.couchdb.nouveau.core.Lucene;
import org.apache.couchdb.nouveau.core.LuceneBundle;

import io.dropwizard.setup.Environment;

public final class Lucene9Bundle<Configuration> implements LuceneBundle<Configuration> {

    private Lucene9 lucene;

    @Override
    public void run(final Configuration configuration, final Environment environment) throws Exception {
        lucene = new Lucene9();
        lucene.setExecutor(environment.lifecycle().executorService("nouveau-lucene9-%d").build());
    }

    @Override
    public Lucene getLucene() {
        return lucene;
    }

}
