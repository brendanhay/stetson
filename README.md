Stetson
=======

[![Build Status](https://secure.travis-ci.org/brendanhay/stetson.png)](http://travis-ci.org/brendanhay/stetson)

Table of Contents
-----------------

* [Configure](#configure)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="configure" />

Configure
---------

The url for the `statsd` instance and the `graphite` namespace prefix are
specified in the `stetson.app.src` env section:

```erlang
{env, [
   {statsd.uri,  <value>},
   {graphite.ns, <value>}
]}
```

Or in your application's `app.config` via:

```erlang
{stetson, [
   {statsd.uri,  <value>},
   {graphite.ns, <value>}
]}
```

Where `<value>` can be one of:

* `'ATOM'`: read as an ENVIRONMENT_VARIABLE
* `"string"`: read as-is


<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/stetson/issues).


<a name="licence" />

Licence
-------

Stetson is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
