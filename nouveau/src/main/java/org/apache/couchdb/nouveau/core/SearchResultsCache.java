package org.apache.couchdb.nouveau.core;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.apache.couchdb.nouveau.api.SearchRequest;
import org.apache.couchdb.nouveau.api.SearchResults;

public class SearchResultsCache {

    record Key(String name, SearchRequest request, long updateSeq, long purgeSeq) {}

    private final Cache<Key, SearchResults> cache;

    public SearchResultsCache(final String cacheSpec) {
        cache = Caffeine.from(cacheSpec).build();
    }

    public SearchResults get(
            final String name, final SearchRequest request, final long updateSeq, final long purgeSeq) {
        return cache.getIfPresent(new Key(name, request, updateSeq, purgeSeq));
    }

    public void put(
            final String name,
            final SearchRequest request,
            final long updateSeq,
            final long purgeSeq,
            final SearchResults results) {
        cache.put(new Key(name, request, updateSeq, purgeSeq), results);
    }
}
