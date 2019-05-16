curl --header "Content-Type: application/json" \
  --request POST \
  --data '[{"id":"52b0e854-a1e1-462b-be1f-ee289ac269f5","conversation_id":"general","user_id": "qwe@qwe.qwe", "content": "Hello World", "date":"2019-05-11 13:45:58.624715Z"}]' \
  http://localhost:4001/history

curl --header "Content-Type: application/json" \
  --request POST \
  --data '[{"id":"52b0e854-a1e1-462b-be1f-ee289ac269f6","conversation_id":"general","user_id": "qwe@qwe.qwe", "content": "Hello World1", "date":"2019-05-11 13:45:58.624715Z"}]' \
  http://localhost:4001/history

curl --header "Content-Type: application/json" \
  --request POST \
  --data '[{"id":"52b0e854-a1e1-462b-be1f-ee289ac269f7","conversation_id":"general","user_id": "qwe@qwe.qwe", "content": "Hello World2", "date":"2019-05-11 13:45:58.624715Z"}]' \
  http://localhost:4001/history



