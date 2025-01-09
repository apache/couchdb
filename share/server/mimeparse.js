// mimeparse.js
//
// This module provides basic functions for handling mime-types. It can
// handle matching mime-types against a list of media-ranges. See section
// 14.1 of the HTTP specification [RFC 2616] for a complete explanation.
//
//   http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1
//
// A port to JavaScript of Joe Gregorio's MIME-Type Parser:
//
//   http://code.google.com/p/mimeparse/
//
// Ported by J. Chris Anderson <jchris@apache.org>, targeting the Spidermonkey runtime.
//
// To run the tests, open mimeparse-js-test.html in a browser.
// Ported from version 0.1.2
// Comments are mostly excerpted from the original.

var Mimeparse = (function() {
  // private helpers
  function strip(string) {
    return string.replace(/^\s+/, '').replace(/\s+$/, '');
  };

  function parseRanges(ranges) {
    var parsedRanges = [], rangeParts = ranges.split(",");
    for (var i=0; i < rangeParts.length; i++) {
      parsedRanges.push(publicMethods.parseMediaRange(rangeParts[i]));
    };
    return parsedRanges;
  };

  var publicMethods = {
    // Carves up a mime-type and returns an Array of the
    //  [type, subtype, params] where "params" is a Hash of all
    //  the parameters for the media range.
    //
    // For example, the media range "application/xhtml;q=0.5" would
    //  get parsed into:
    //
    // ["application", "xhtml", { "q" : "0.5" }]
    parseMimeType : function(mimeType) {
      var fullType, typeParts, params = {}, parts = mimeType.split(';');
      for (var i=0; i < parts.length; i++) {
        var p = parts[i].split('=');
        if (p.length == 2) {
          params[strip(p[0])] = strip(p[1]);
        }
      };
      fullType = parts[0].replace(/^\s+/, '').replace(/\s+$/, '');
      if (fullType == '*') fullType = '*/*';
      typeParts = fullType.split('/');
      return [typeParts[0], typeParts[1], params];
    },

    // Carves up a media range and returns an Array of the
    //  [type, subtype, params] where "params" is a Object with
    //  all the parameters for the media range.
    //
    // For example, the media range "application/*;q=0.5" would
    //  get parsed into:
    //
    // ["application", "*", { "q" : "0.5" }]
    //
    // In addition this function also guarantees that there
    //  is a value for "q" in the params dictionary, filling it
    //  in with a proper default if necessary.
    parseMediaRange : function(range) {
      var q, parsedType = this.parseMimeType(range);
      if (!parsedType[2]['q']) {
        parsedType[2]['q'] = '1';
      } else {
        q = parseFloat(parsedType[2]['q']);
        if (isNaN(q)) {
          parsedType[2]['q'] = '1';
        } else if (q > 1 || q < 0) {
          parsedType[2]['q'] = '1';
        }
      }
      return parsedType;
    },

    // Find the best match for a given mime-type against
    // a list of media_ranges that have already been
    // parsed by parseMediaRange(). Returns an array of
    // the fitness value and the value of the 'q' quality
    // parameter of the best match, or (-1, 0) if no match
    // was found. Just as for qualityParsed(), 'parsed_ranges'
    // must be a list of parsed media ranges.
    fitnessAndQualityParsed : function(mimeType, parsedRanges) {
      var bestFitness = -1, bestFitQ = 0, target = this.parseMediaRange(mimeType);
      var targetType = target[0], targetSubtype = target[1], targetParams = target[2];

      for (var i=0; i < parsedRanges.length; i++) {
        var parsed = parsedRanges[i];
        var type = parsed[0], subtype = parsed[1], params = parsed[2];
        if ((type == targetType || type == "*" || targetType == "*") &&
          (subtype == targetSubtype || subtype == "*" || targetSubtype == "*")) {
          var matchCount = 0;
          for (var param in targetParams) {
            if (param != 'q' && params[param] && params[param] == targetParams[param]) {
              matchCount += 1;
            }
          }

          var fitness = (type == targetType) ? 100 : 0;
          fitness += (subtype == targetSubtype) ? 10 : 0;
          fitness += matchCount;

          if (fitness > bestFitness) {
            bestFitness = fitness;
            bestFitQ = params["q"];
          }
        }
      };
      return [bestFitness, parseFloat(bestFitQ)];
    },

    // Find the best match for a given mime-type against
    // a list of media_ranges that have already been
    // parsed by parseMediaRange(). Returns the
    // 'q' quality parameter of the best match, 0 if no
    // match was found. This function bahaves the same as quality()
    // except that 'parsedRanges' must be a list of
    // parsed media ranges.
    qualityParsed : function(mimeType, parsedRanges) {
      return this.fitnessAndQualityParsed(mimeType, parsedRanges)[1];
    },

    // Returns the quality 'q' of a mime-type when compared
    // against the media-ranges in ranges. For example:
    //
    // >>> Mimeparse.quality('text/html','text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5')
    // 0.7
    quality : function(mimeType, ranges) {
      return this.qualityParsed(mimeType, parseRanges(ranges));
    },

    // Takes a list of supported mime-types and finds the best
    // match for all the media-ranges listed in header. The value of
    // header must be a string that conforms to the format of the
    // HTTP Accept: header. The value of 'supported' is a list of
    // mime-types.
    //
    // >>> bestMatch(['application/xbel+xml', 'text/xml'], 'text/*;q=0.5,*/*; q=0.1')
    // 'text/xml'
    bestMatch : function(supported, header) {
      var parsedHeader = parseRanges(header);
      var weighted = [];
      for (var i=0; i < supported.length; i++) {
        weighted.push([publicMethods.fitnessAndQualityParsed(supported[i], parsedHeader), i, supported[i]]);
      };
      weighted.sort();
      return weighted[weighted.length-1][0][1] ? weighted[weighted.length-1][2] : '';
    }
  };
  return publicMethods;
})();
