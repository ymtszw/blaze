# Blaze

Kindle recommender.

## Development

Install [DynamoDBLocal](http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DynamoDBLocal.html):

```sh
sudo mkdir /opt/dynamodb_local
sudo chown $(whoami) /opt/dynamodb_local
cd /opt/dynamodb_local
wget https://s3-ap-northeast-1.amazonaws.com/dynamodb-local-tokyo/dynamodb_local_latest.tar.gz
tar xvf dynamodb_local_latest.tar.gz
```

Setup:

```sh
npm install
npm install -g elm-format
```

Use `elm-format`, `eslint` and `stylelint` with your editor.
