//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.apache.couchdb.nouveau;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ServiceLoader;
import java.util.concurrent.ScheduledExecutorService;

import org.apache.couchdb.nouveau.core.IndexManager;
import org.apache.couchdb.nouveau.core.UpdatesOutOfOrderExceptionMapper;

import io.dropwizard.Application;
import io.dropwizard.ConfiguredBundle;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

public class NouveauApplication extends Application<NouveauApplicationConfiguration> {

    private IndexManager indexManager;

    public static void main(String[] args) throws Exception {
        new NouveauApplication().run(args);
    }

    @Override
    public String getName() {
        return "Nouveau";
    }

    @Override
    public void initialize(Bootstrap<NouveauApplicationConfiguration> bootstrap) {
        indexManager = new IndexManager();

        // Find Lucene bundles
        for (String name : System.getProperties().stringPropertyNames()) {
            if (name.startsWith("nouveau.bundle.")) {
                try {
                    ClassLoader classLoader = URLClassLoader
                            .newInstance(new URL[] { new URL(System.getProperty(name)) });
                    final ServiceLoader<ConfiguredBundle> bundleLoader = ServiceLoader.load(ConfiguredBundle.class,
                            classLoader);
                    for (final ConfiguredBundle<NouveauApplicationConfiguration> bundle : bundleLoader) {
                        if (bundle instanceof LuceneBundle) {
                            ((LuceneBundle)bundle).setIndexManager(indexManager);
                            bootstrap.addBundle(bundle);
                        }
                    }
                } catch (final MalformedURLException e) {
                    throw new Error(e);
                }
            }
        }
    }

    @Override
    public void run(NouveauApplicationConfiguration configuration, Environment environment) throws Exception {
        environment.jersey().register(new UpdatesOutOfOrderExceptionMapper());

        // Configure index manager
        final ScheduledExecutorService indexManagerScheduler = environment.lifecycle()
                .scheduledExecutorService("index-manager-scheduler-%d")
                .threads(10)
                .build();
        indexManager.setCommitIntervalSeconds(configuration.getCommitIntervalSeconds());
        indexManager.setIdleSeconds(configuration.getIdleSeconds());
        indexManager.setMaxIndexesOpen(configuration.getMaxIndexesOpen());
        indexManager.setObjectMapper(environment.getObjectMapper());
        indexManager.setRootDir(configuration.getRootDir());
        indexManager.setScheduler(indexManagerScheduler);
        environment.lifecycle().manage(indexManager);
    }

}
