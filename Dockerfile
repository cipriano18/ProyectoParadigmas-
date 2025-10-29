# Usa la imagen oficial de Prolog
FROM swipl:latest

# Crea un directorio de trabajo
WORKDIR /app

# Copia los archivos del proyecto al contenedor
COPY . /app

# Expone el puerto 8080
EXPOSE 8080

# Ejecuta tu servidor Prolog
CMD ["swipl", "-s", "api.pl", "-g", "start_server(8080)", "-t", "halt"]
