# pequod-plus

An improved edition of pequod-cljs, the participatory planning procedure prototype (in Clojure and Clojurescript).

To learn more about the model of a participatory economy, visit [participatoryeconomy.org](https://www.participatoryeconomy.org).

```
lein new reagent pequod-plus +test +figwheel +devcards +cider

TODO:
+ Build cljc tests
+ Build middleware logic (Clojurey!)

```

## Quickstart

These are the instructions

1.  Generate worker councils and consumer councils:

```
lein run -m pequod-plus.gen [NAME-SPACE-TO-USE]
```

For example:

```
time lein run -m pequod-plus.gen ppex001 > ppex001.cljs

time lein run -m pequod-plus.gen-revised ppex004 > ppex004.clj
```

2.  Move the councils into `src/cljs/pequod_plus`

```
mv ppex001.cljs src/cljs/pequod_plus/ppex001.cljs
```

3.  Run `lein figwheel`, as listed below.

## CSVGen

```
rm results004.csv; time lein run -m pequod-plus.csvgen ppex004 > ppex004.csv
```

## Development mode

To start the Figwheel compiler, navigate to the project folder and run the following command in the terminal:

```
lein figwheel
```

To start the [DevCards](https://github.com/bhauman/devcards) build, run

```
lein figwheel devcards
```

Figwheel will automatically push cljs changes to the browser. The server will be available at [http://localhost:3449](http://localhost:3449) once Figwheel starts up.  To view your devcards, type `(switch-to-build devcards)` at the Figwheel REPL and navigate to [http://localhost:3449/cards](http://localhost:3449/cards). 

Figwheel also starts `nREPL` using the value of the `:nrepl-port` in the `:figwheel`
config found in `project.clj`. By default the port is set to `7002`.

The figwheel server can have unexpected behaviors in some situations such as when using
websockets. In this case it's recommended to run a standalone instance of a web server as follows:

```
lein do clean, run
```

The application will now be available at [http://localhost:3000](http://localhost:3000).


### Optional development tools

Start the browser REPL:

```
$ lein repl
```
The Jetty server can be started by running:

```clojure
(start-server)
```
and stopped by running:
```clojure
(stop-server)
```

## Running the tests
To run [cljs.test](https://github.com/clojure/clojurescript/blob/master/src/main/cljs/cljs/test.cljs) tests using headless chrome install karma and its plugins:

```
npm install -g karma-cli
npm install karma karma-cljs-test karma-chrome-launcher --save-dev
lein doo chrome-headless test once
```

For other environments please check [doo's documentation](https://github.com/bensu/doo#setting-up-environments).


For installation instructions of PhantomJS, please see [this](http://phantomjs.org/download.html).

## Building for release

```
lein do clean, uberjar
```

## Deploying to Heroku

Make sure you have [Git](http://git-scm.com/downloads) and [Heroku toolbelt](https://toolbelt.heroku.com/) installed, then simply follow the steps below.

Optionally, test that your application runs locally with foreman by running.

```
foreman start
```

Now, you can initialize your git repo and commit your application.

```
git init
git add .
git commit -m "init"
```
create your app on Heroku

```
heroku create
```

optionally, create a database for the application

```
heroku addons:add heroku-postgresql
```

The connection settings can be found at your [Heroku dashboard](https://dashboard.heroku.com/apps/) under the add-ons for the app.

deploy the application

```
git push heroku master
```

Your application should now be deployed to Heroku!
For further instructions see the [official documentation](https://devcenter.heroku.com/articles/clojure).
