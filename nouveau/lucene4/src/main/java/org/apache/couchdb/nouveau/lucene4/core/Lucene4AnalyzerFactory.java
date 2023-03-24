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

package org.apache.couchdb.nouveau.lucene4.core;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response.Status;

import org.apache.couchdb.nouveau.api.IndexDefinition;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.ar.ArabicAnalyzer;
import org.apache.lucene.analysis.bg.BulgarianAnalyzer;
import org.apache.lucene.analysis.ca.CatalanAnalyzer;
import org.apache.lucene.analysis.cjk.CJKAnalyzer;
import org.apache.lucene.analysis.standard.ClassicAnalyzer;
import org.apache.lucene.analysis.cn.smart.SmartChineseAnalyzer;
import org.apache.lucene.analysis.core.KeywordAnalyzer;
import org.apache.lucene.analysis.core.SimpleAnalyzer;
import org.apache.lucene.analysis.core.WhitespaceAnalyzer;
import org.apache.lucene.analysis.cz.CzechAnalyzer;
import org.apache.lucene.analysis.da.DanishAnalyzer;
import org.apache.lucene.analysis.de.GermanAnalyzer;
import org.apache.lucene.analysis.standard.UAX29URLEmailAnalyzer;
import org.apache.lucene.analysis.en.EnglishAnalyzer;
import org.apache.lucene.analysis.es.SpanishAnalyzer;
import org.apache.lucene.analysis.eu.BasqueAnalyzer;
import org.apache.lucene.analysis.fa.PersianAnalyzer;
import org.apache.lucene.analysis.fi.FinnishAnalyzer;
import org.apache.lucene.analysis.fr.FrenchAnalyzer;
import org.apache.lucene.analysis.ga.IrishAnalyzer;
import org.apache.lucene.analysis.gl.GalicianAnalyzer;
import org.apache.lucene.analysis.hi.HindiAnalyzer;
import org.apache.lucene.analysis.hu.HungarianAnalyzer;
import org.apache.lucene.analysis.hy.ArmenianAnalyzer;
import org.apache.lucene.analysis.id.IndonesianAnalyzer;
import org.apache.lucene.analysis.it.ItalianAnalyzer;
import org.apache.lucene.analysis.ja.JapaneseAnalyzer;
import org.apache.lucene.analysis.lv.LatvianAnalyzer;
import org.apache.lucene.analysis.nl.DutchAnalyzer;
import org.apache.lucene.analysis.no.NorwegianAnalyzer;
import org.apache.lucene.analysis.pl.PolishAnalyzer;
import org.apache.lucene.analysis.pt.PortugueseAnalyzer;
import org.apache.lucene.analysis.ro.RomanianAnalyzer;
import org.apache.lucene.analysis.ru.RussianAnalyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.analysis.sv.SwedishAnalyzer;
import org.apache.lucene.analysis.th.ThaiAnalyzer;
import org.apache.lucene.analysis.tr.TurkishAnalyzer;

public final class Lucene4AnalyzerFactory {

    public Lucene4AnalyzerFactory() {
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
        return new PerFieldAnalyzer(defaultAnalyzer, fieldAnalyzers);
    }

    private enum KnownAnalyzer {
        // TODO support stopword list configuration
        arabic(() -> new ArabicAnalyzer(Utils.LUCENE_VERSION)),
        armenian(() -> new ArmenianAnalyzer(Utils.LUCENE_VERSION)),
        basque(() -> new BasqueAnalyzer(Utils.LUCENE_VERSION)),
        bulgarian(() -> new BulgarianAnalyzer(Utils.LUCENE_VERSION)),
        catalan(() -> new CatalanAnalyzer(Utils.LUCENE_VERSION)),
        chinese(() -> new SmartChineseAnalyzer(Utils.LUCENE_VERSION)),
        cjk(() -> new CJKAnalyzer(Utils.LUCENE_VERSION)),
        classic(() -> new ClassicAnalyzer(Utils.LUCENE_VERSION)),
        czech(() -> new CzechAnalyzer(Utils.LUCENE_VERSION)),
        danish(() -> new DanishAnalyzer(Utils.LUCENE_VERSION)),
        dutch(() -> new DutchAnalyzer(Utils.LUCENE_VERSION)),
        email(() -> new UAX29URLEmailAnalyzer(Utils.LUCENE_VERSION)),
        english(() -> new EnglishAnalyzer(Utils.LUCENE_VERSION)),
        finnish(() -> new FinnishAnalyzer(Utils.LUCENE_VERSION)),
        french(() -> new FrenchAnalyzer(Utils.LUCENE_VERSION)),
        galician(() -> new GalicianAnalyzer(Utils.LUCENE_VERSION)),
        german(() -> new GermanAnalyzer(Utils.LUCENE_VERSION)),
        hindi(() -> new HindiAnalyzer(Utils.LUCENE_VERSION)),
        hungarian(() -> new HungarianAnalyzer(Utils.LUCENE_VERSION)),
        indonesian(() -> new IndonesianAnalyzer(Utils.LUCENE_VERSION)),
        irish(() -> new IrishAnalyzer(Utils.LUCENE_VERSION)),
        italian(() -> new ItalianAnalyzer(Utils.LUCENE_VERSION)),
        japanese(() -> new JapaneseAnalyzer(Utils.LUCENE_VERSION)),
        keyword(() -> new KeywordAnalyzer()),
        latvian(() -> new LatvianAnalyzer(Utils.LUCENE_VERSION)),
        norwegian(() -> new NorwegianAnalyzer(Utils.LUCENE_VERSION)),
        persian(() -> new PersianAnalyzer(Utils.LUCENE_VERSION)),
        polish(() -> new PolishAnalyzer(Utils.LUCENE_VERSION)),
        portugese(() -> new PortugueseAnalyzer(Utils.LUCENE_VERSION)),
        romanian(() -> new RomanianAnalyzer(Utils.LUCENE_VERSION)),
        russian(() -> new RussianAnalyzer(Utils.LUCENE_VERSION)),
        simple(() -> new SimpleAnalyzer(Utils.LUCENE_VERSION)),
        simple_asciifolding(() -> new SimpleAsciiFoldingAnalyzer(Utils.LUCENE_VERSION)),
        spanish(() -> new SpanishAnalyzer(Utils.LUCENE_VERSION)),
        standard(() -> new StandardAnalyzer(Utils.LUCENE_VERSION)),
        swedish(() -> new SwedishAnalyzer(Utils.LUCENE_VERSION)),
        thai(() -> new ThaiAnalyzer(Utils.LUCENE_VERSION)),
        turkish(() -> new TurkishAnalyzer(Utils.LUCENE_VERSION)),
        whitespace(() -> new WhitespaceAnalyzer(Utils.LUCENE_VERSION));

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
