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

package org.apache.couchdb.nouveau.api;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;

@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class Range<T> {

    @NotEmpty
    private String label;

    @NotNull
    private T min;

    private boolean minInclusive = true;

    @NotNull
    private T max;

    private boolean maxInclusive = true;

    public Range() {}

    public Range(String label, T min, boolean minInclusive, T max, boolean maxInclusive) {
        this.label = label;
        this.min = min;
        this.minInclusive = minInclusive;
        this.max = max;
        this.maxInclusive = maxInclusive;
    }

    @JsonProperty
    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    @JsonProperty
    public T getMin() {
        return min;
    }

    public void setMin(T min) {
        this.min = min;
    }

    @JsonProperty("min_inclusive")
    public boolean isMinInclusive() {
        return minInclusive;
    }

    public void setMinInclusive(boolean minInclusive) {
        this.minInclusive = minInclusive;
    }

    @JsonProperty
    public T getMax() {
        return max;
    }

    public void setMax(T max) {
        this.max = max;
    }

    @JsonProperty("max_inclusive")
    public boolean isMaxInclusive() {
        return maxInclusive;
    }

    public void setMaxInclusive(boolean maxInclusive) {
        this.maxInclusive = maxInclusive;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((label == null) ? 0 : label.hashCode());
        result = prime * result + ((min == null) ? 0 : min.hashCode());
        result = prime * result + (minInclusive ? 1231 : 1237);
        result = prime * result + ((max == null) ? 0 : max.hashCode());
        result = prime * result + (maxInclusive ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        Range<?> other = (Range<?>) obj;
        if (label == null) {
            if (other.label != null) return false;
        } else if (!label.equals(other.label)) return false;
        if (min == null) {
            if (other.min != null) return false;
        } else if (!min.equals(other.min)) return false;
        if (minInclusive != other.minInclusive) return false;
        if (max == null) {
            if (other.max != null) return false;
        } else if (!max.equals(other.max)) return false;
        if (maxInclusive != other.maxInclusive) return false;
        return true;
    }

    @Override
    public String toString() {
        return "Range [label=" + label + ", min=" + min + ", minInclusive=" + minInclusive + ", max=" + max
                + ", maxInclusive=" + maxInclusive + "]";
    }
}
