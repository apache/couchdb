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

package org.apache.couchdb.nouveau.core;

import java.io.FileNotFoundException;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;

import io.dropwizard.jersey.errors.ErrorMessage;

public class FileNotFoundExceptionMapper implements ExceptionMapper<FileNotFoundException> {

    @Override
    public Response toResponse(final FileNotFoundException exception) {
        return Response.status(Status.NOT_FOUND)
            .type(MediaType.APPLICATION_JSON_TYPE)
            .entity(new ErrorMessage(Status.NOT_FOUND.getStatusCode(), "Index does not exist"))
            .build();
    }

}
