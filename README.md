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

Nginx config:

```conf
...
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
$ docker run -i -t --rm -p 3000:3000 -e NODE_ENV=production -e LW_SUBDOMAIN=login.servant-and-login-with.com:3433 -e LW_SESSION_SECRET=foobar -e LW_JWT_SECRET=foobarbaz -e LW_TWITTER_CONSUMERKEY=lT...hC -e LW_TWITTER_CONSUMERSECRET=q0...WK lipp/login-with
```

Visit login url with browser:

```sh
$ firefox 'https://login.servant-and-login-with.com:3443/twitter?success=https://servant-and-login-with.com:8443/after-login&failure=http://google.com'
```
