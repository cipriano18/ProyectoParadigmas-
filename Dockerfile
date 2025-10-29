FROM swipl:latest
WORKDIR /app
COPY . /app
EXPOSE 8080
CMD ["swipl", "-s", "api.pl", "-g", "start_server", "-t", "halt"]
