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
//
// Based on the normalizeFunction which can be
// found here:
//
//  https://github.com/dmunch/couch-chakra/blob/master/js/normalizeFunction.js

function rewriteFunInt(fun) {
    const ast = esprima.parse(fun);
    let idx = ast.body.length - 1;
    let decl = {};

    // Search for the first FunctionDeclaration beginning from the end
    do {
        decl = ast.body[idx--];
    } while (idx >= 0 && decl.type !== "FunctionDeclaration");
    idx++;

    // If we have a function declaration without an Id, wrap it
    // in an ExpressionStatement and change it into
    // a FuntionExpression
    if (decl.type == "FunctionDeclaration" && decl.id == null) {
        decl.type = "FunctionExpression";
        ast.body[idx] = {
            type: "ExpressionStatement",
            expression: decl
        };
    }

    // Generate source from the rewritten AST
    return escodegen.generate(ast);
}


function rewriteFun(funJSON) {
    const fun = JSON.parse(funJSON);
    return JSON.stringify(rewriteFunInt(fun));
}

function rewriteFuns(funsJSON) {
    let funs = JSON.parse(funsJSON);
    const results = Array.from(funs, (fun) => {
        return rewriteFunInt(fun);
    });
    return JSON.stringify(results);
}