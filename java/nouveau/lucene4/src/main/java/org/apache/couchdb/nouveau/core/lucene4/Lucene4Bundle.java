package org.apache.couchdb.nouveau.core.lucene4;

import org.apache.couchdb.nouveau.core.Lucene;
import org.apache.couchdb.nouveau.core.LuceneBundle;

import io.dropwizard.setup.Environment;

public final class Lucene4Bundle<Configuration> implements LuceneBundle<Configuration> {

    private Lucene4 lucene;

    @Override
    public void run(final Configuration configuration, final Environment environment) throws Exception {
        lucene = new Lucene4();
        lucene.setExecutor(environment.lifecycle().executorService("nouveau-lucene4-%d").build());
    }

    @Override
    public Lucene getLucene() {
        return lucene;
    }

}
