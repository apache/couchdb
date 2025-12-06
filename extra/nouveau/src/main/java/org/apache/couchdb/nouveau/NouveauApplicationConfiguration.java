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

import com.fasterxml.jackson.annotation.JsonProperty;
import io.dropwizard.core.Configuration;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import java.nio.file.Path;

public class NouveauApplicationConfiguration extends Configuration {

    @Min(10)
    private int maxIndexesOpen = 10;

    @Min(10)
    private int commitIntervalSeconds = 10;

    @Min(30)
    private int idleSeconds = 30;

    @NotNull
    private Path rootDir = null;

    @Min(2)
    private int schedulerThreadCount = 5;

    @JsonProperty
    public void setMaxIndexesOpen(int maxIndexesOpen) {
        this.maxIndexesOpen = maxIndexesOpen;
    }

    public int getMaxIndexesOpen() {
        return maxIndexesOpen;
    }

    @JsonProperty
    public void setCommitIntervalSeconds(int commitIntervalSeconds) {
        this.commitIntervalSeconds = commitIntervalSeconds;
    }

    public int getCommitIntervalSeconds() {
        return commitIntervalSeconds;
    }

    @JsonProperty
    public void setIdleSeconds(int idleSeconds) {
        this.idleSeconds = idleSeconds;
    }

    public int getIdleSeconds() {
        return idleSeconds;
    }

    @JsonProperty
    public void setRootDir(Path rootDir) {
        this.rootDir = rootDir;
    }

    public Path getRootDir() {
        return rootDir;
    }

    public int getSchedulerThreadCount() {
        return schedulerThreadCount;
    }

    public void setSchedulerThreadCount(int schedulerThreadCount) {
        this.schedulerThreadCount = schedulerThreadCount;
    }
}
