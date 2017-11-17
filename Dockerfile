FROM node:alpine
LABEL maintainer="mike.ralphson@gmail.com" description="OpenAPI 2.0/3.0 CodeGen"
ENV NODE_ENV=production
WORKDIR /app

# install deps first (enables layer reuse)
COPY package.json .
RUN apk update && apk upgrade && \
    apk add --no-cache bash git openssh
RUN npm config set cache /tmp && npm i && rm -rf /tmp/*

# now load the app source
COPY . .
EXPOSE 3000
CMD [ "node", "cg", "--help" ]
