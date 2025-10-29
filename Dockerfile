FROM swipl:latest
WORKDIR /app
COPY . /app

EXPOSE 8080

CMD ["swipl", "-q", "-s", "supermarket.pl", "-t", "halt"]
