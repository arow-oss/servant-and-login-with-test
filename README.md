# servant-and-login-with-test

Add hosts to `/etc/hosts`:

```sh
$ cat /etc/hosts
127.0.0.1       localhost servant-and-login-with.com login.servant-and-login-with.com
127.0.1.1       debian-dev

# The following lines are desirable for IPv6 capable hosts
::1     localhost ip6-localhost ip6-loopback
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters
```

Install nginx:

```sh
$ sudo apt-get install nginx
```

Create certs:

```sh
$ cd /etc/nginx
$ sudo mkdir certs
$ cd certs
$ sudo openssl req -new -x509 -days 365 -nodes -out servant-and-login-with.pem -keyout servant-and-login-with.key
```

Create the following Nginx config:

```conf
$ cat /etc/nginx/sites-available/servant-and-login-with
server {
  listen 3443 ssl;
  server_name login.servant-and-login-with.com;

  # Path to our ssl certificate.
  ssl_certificate /etc/nginx/certs/servant-and-login-with.pem;
  ssl_certificate_key /etc/nginx/certs/servant-and-login-with.key;

  # Set where our access logs will be stored.
  access_log /var/log/nginx/login-with.log combined;

  location / {
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_pass http://login-with;
  }
}

upstream login-with {
  server localhost:3000;
}

server {
  listen 8443 ssl;
  server_name servant-and-login-with.com;

  # Path to our ssl certificate.
  ssl_certificate /etc/nginx/certs/servant-and-login-with.pem;
  ssl_certificate_key /etc/nginx/certs/servant-and-login-with.key;

  # Set where our access logs will be stored.
  access_log /var/log/nginx/servant.log combined;

  location / {
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
    proxy_pass http://servant;
  }
}

upstream servant {
  server localhost:8000;
}
```

Enable nginx config:

```sh
$ cd /etc/nginx/sites-enabled
$ ln -sf ../sites-available/servant-and-login-with
```

Check nginx config syntax and restart nginx:

```sh
$ nginx -t
```

Run login-with:

```sh
$ docker run -i -t --rm -p 3000:3000 -e NODE_ENV=production -e LW_SUBDOMAIN=login.servant-and-login-with.com:3443 -e LW_COOKIE_DOMAIN=.servant-and-login-with.com -e LW_SESSION_SECRET=foobar -e LW_JWT_SECRET=foobarbaz -e LW_TWITTER_CONSUMERKEY=lT...hC -e LW_TWITTER_CONSUMERSECRET=q0...WK lipp/login-with
```

Run haskell app:

```sh
$ stack build --fast
$ stack exec -- servant-and-login-with-test-exe
```

Visit login url with browser:

```sh
$ firefox 'https://login.servant-and-login-with.com:3443/twitter?success=https://servant-and-login-with.com:8443/after-login&failure=http://google.com'
```
