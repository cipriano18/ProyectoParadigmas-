FROM swipl:latest
WORKDIR /app
COPY . /app

EXPOSE 8080

# Ejecuta Prolog sin modo interactivo y lanza supermarket.pl
CMD ["swipl", "-q", "-s", "supermarket.pl", "-g", "start", "-t", "halt"]
