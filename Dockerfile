FROM swipl:latest
WORKDIR /app
COPY . /app

# Expone el puerto que Railway usa
EXPOSE 8080

# Evita modo interactivo y arranca directamente el servidor
CMD ["swipl", "-q", "-g", "start", "-t", "halt", "supermarket.pl"]
