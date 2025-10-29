
FROM swipl:latest
WORKDIR /app
COPY . /app
EXPOSE 8080
CMD ["swipl", "-s", "supermarket.pl", "-t", "halt"]
