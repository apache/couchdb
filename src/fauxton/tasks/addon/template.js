// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

'use strict';

exports.description = 'Generate a skeleton for an addon.';

exports.notes = '';

exports.after = "Created your addon! Don't forget to update your"+
                " settings.json for it to be compiled and deployed";

// Any existing file or directory matching this wildcard will cause a warning.
// exports.warnOn = '*';

// The actual init template.
exports.template = function(grunt, init, done) {

  // destpath
  init.process(
    {},
    [
      {
        name: "name",
        message: "Add on Name",
        validator: /^[\w\-\.]+$/,
        default: "WickedCool"
      },
      {
        name: "path",
        message: "Location of add ons",
        default: "app/addons"
      },
      {
        name: "assets",
        message: "Do you need an assets folder? (for .less)",
        default: 'y/N'
      }
    ],
    function (err, props) {
      // Files to copy (and process).
      var files = init.filesToCopy(props);

      // Actually copy and process (apply the template props) files.
      init.copyAndProcess(files, props);

      // Make the assets dir if requested
      if (props.assets == "y"){
        var asspath = props.path + "/" + props.name.toLowerCase() + "/assets";
        grunt.file.mkdir(asspath);
        grunt.log.writeln("Created " + asspath);
      }

      var tmplpath = props.path + "/" + props.name.toLowerCase() + "/templates";
      grunt.file.mkdir(tmplpath);
      grunt.log.writeln("Created " + tmplpath);
      // All done!
      done();
    }
  )
};