
#!/bin/bash

# Configuration
DB_URL="http://127.0.0.1:15984"
DB_NAME="test_all_docs_range"

# Create a database
curl -X PUT "$DB_URL/$DB_NAME" -u admin:password

# Insert some documents
curl -X PUT "$DB_URL/$DB_NAME/a" -d '{}' -u admin:password
curl -X PUT "$DB_URL/$DB_NAME/z" -d '{}' -u admin:password

# Test case 1: Inverted range (start_key > end_key)
# Should return 400 Bad Request if fixed. Currently returns empty list or ignores.
echo "Testing inverted range (startkey='z', endkey='a')..."
response=$(curl -s -o /dev/null -w "%{http_code}" -g "$DB_URL/$DB_NAME/_all_docs?startkey=\"z\"&endkey=\"a\"" -u admin:password)

if [ "$response" == "400" ]; then
  echo "SUCCESS: Server returned 400 for inverted range."
else
  echo "FAILURE: Server returned $response for inverted range. Expected 400."
fi

# Test case 2: Normal range
echo "Testing normal range (startkey='a', endkey='z')..."
response=$(curl -s -o /dev/null -w "%{http_code}" -g "$DB_URL/$DB_NAME/_all_docs?startkey=\"a\"&endkey=\"z\"" -u admin:password)

if [ "$response" == "200" ]; then
  echo "SUCCESS: Server returned 200 for normal range."
else
  echo "FAILURE: Server returned $response for normal range. Expected 200."
fi

# Cleanup
curl -X DELETE "$DB_URL/$DB_NAME" -u admin:password
