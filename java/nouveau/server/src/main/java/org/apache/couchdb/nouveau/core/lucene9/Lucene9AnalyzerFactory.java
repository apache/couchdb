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

package org.apache.couchdb.nouveau.core.lucene9;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.Analyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.ar.ArabicAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.bg.BulgarianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.ca.CatalanAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.cjk.CJKAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.classic.ClassicAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.cn.smart.SmartChineseAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.core.KeywordAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.core.SimpleAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.core.WhitespaceAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.cz.CzechAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.da.DanishAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.de.GermanAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.email.UAX29URLEmailAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.en.EnglishAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.es.SpanishAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.eu.BasqueAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.fa.PersianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.fi.FinnishAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.fr.FrenchAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.ga.IrishAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.gl.GalicianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.hi.HindiAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.hu.HungarianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.hy.ArmenianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.id.IndonesianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.it.ItalianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.ja.JapaneseAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.lv.LatvianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.miscellaneous.PerFieldAnalyzerWrapper;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.nl.DutchAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.no.NorwegianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.pl.PolishAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.pt.PortugueseAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.ru.RussianAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.standard.StandardAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.sv.SwedishAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.th.ThaiAnalyzer;
import org.apache.couchdb.nouveau.lucene9.lucene.analysis.tr.TurkishAnalyzer;

final class Lucene9AnalyzerFactory {

    public Lucene9AnalyzerFactory() {
    }

    public static Analyzer fromDefinition(final IndexDefinition indexDefinition) {
        final Analyzer defaultAnalyzer = newAnalyzer(indexDefinition.getDefaultAnalyzer());
        if (!indexDefinition.hasFieldAnalyzers()) {
            return defaultAnalyzer;
        }
        final Map<String, Analyzer> fieldAnalyzers = new HashMap<String, Analyzer>();
        for (Map.Entry<String, String> entry : indexDefinition.getFieldAnalyzers().entrySet()) {
            fieldAnalyzers.put(entry.getKey(), newAnalyzer(entry.getValue()));
        }
        return new PerFieldAnalyzerWrapper(defaultAnalyzer, fieldAnalyzers);
    }

    private enum KnownAnalyzer {

        arabic(() -> new ArabicAnalyzer()),
        armenian(() -> new ArmenianAnalyzer()),
        basque(() -> new BasqueAnalyzer()),
        bulgarian(() -> new BulgarianAnalyzer()),
        catalan(() -> new CatalanAnalyzer()),
        chinese(() -> new SmartChineseAnalyzer()),
        cjk(() -> new CJKAnalyzer()),
        classic(() -> new ClassicAnalyzer()),
        czech(() -> new CzechAnalyzer()),
        danish(() -> new DanishAnalyzer()),
        dutch(() -> new DutchAnalyzer()),
        email(() -> new UAX29URLEmailAnalyzer()),
        english(() -> new EnglishAnalyzer()),
        finnish(() -> new FinnishAnalyzer()),
        french(() -> new FrenchAnalyzer()),
        galician(() -> new GalicianAnalyzer()),
        german(() -> new GermanAnalyzer()),
        hindi(() -> new HindiAnalyzer()),
        hungarian(() -> new HungarianAnalyzer()),
        indonesian(() -> new IndonesianAnalyzer()),
        irish(() -> new IrishAnalyzer()),
        italian(() -> new ItalianAnalyzer()),
        japanese(() -> new JapaneseAnalyzer()),
        keyword(() -> new KeywordAnalyzer()),
        latvian(() -> new LatvianAnalyzer()),
        norwegian(() -> new NorwegianAnalyzer()),
        persian(() -> new PersianAnalyzer()),
        polish(() -> new PolishAnalyzer()),
        portugese(() -> new PortugueseAnalyzer()),
        romanian(() -> new RomanianAnalyzer()),
        russian(() -> new RussianAnalyzer()),
        simple(() -> new SimpleAnalyzer()),
        spanish(() -> new SpanishAnalyzer()),
        standard(() -> new StandardAnalyzer()),
        swedish(() -> new SwedishAnalyzer()),
        thai(() -> new ThaiAnalyzer()),
        turkish(() -> new TurkishAnalyzer()),
        whitespace(() -> new WhitespaceAnalyzer());

        private final Supplier<? extends Analyzer> supplier;

        private KnownAnalyzer(final Supplier<? extends Analyzer> supplier) {
            this.supplier = supplier;
        }

        private Analyzer newInstance() {
            return supplier.get();
        }
    }

    public static Analyzer newAnalyzer(final String name) {
        try {
            return KnownAnalyzer.valueOf(name).newInstance();
        } catch (IllegalArgumentException e) {
            throw new WebApplicationException(name + " is not a valid analyzer name", Status.BAD_REQUEST);
        }
    }

}
