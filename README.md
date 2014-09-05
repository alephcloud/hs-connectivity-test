[![Build Status](https://travis-ci.org/alephcloud/hs-connectivity-test.svg?branch=master)](https://travis-ci.org/alephcloud/hs-connectivity-test)

This package provides tools (a library as well as an executable) for testing
network connectivity.

Currently the following tests are supported:

* Socket allocation,
* Name resolution,
* TCP connection,
* ICMP ping,
* ICMP traceroute, and
* UDP connection.

On UNIX systems the ICMP and UDP based tests require elevated (root) rights. On
Windows systems this is not required for ICMP.

In the current version only IPv4 is supported.

Installation
============

This package requires a recent version of *Cabal* and *cabal-install*
(at least 1.20). If you have an older verison you must update *Cabal* and
*cabal-install* first by running:

~~~
cabal update
cabal install cabal-install
~~~

For installation from [Hackage](https://hackage.haskell.org/package/connectivity-test)
run

~~~
cabal install connectivity-test
~~~

For installation from [GitHub](https://github.com/alephcloud/hs-connectivity-test)
run

~~~
git clone https://github.com/alephcloud/hs-connectivity-test
cd hs-connectivity-test
cabal install
~~~

Usage
=====

For usage of the library please refer to the
[API documentation](https://hackage.haskell.org/package/connectivity-test).

For usage of the application you may call the binary with the option `--help`:

~~~
connectivity-test --help
~~~

Which will print:

    Usage: connectivity-test [-i|--info] [--long-info] [-v|--version] [--license]
                             [-?|-h|--help] [--type tcp|udp|icmp] [--host HOSTNAME]
                             [-p|--port PORT] [--ttl INT] [-t|--timeout FLOAT]
                             [--trace] [-p|--print-config] [-c|--config-file FILE]
      Connection Test

    Available options:
      -i,--info                Print program info message and exit
      --long-info              Print detailed program info message and exit
      -v,--version             Print version string and exit
      --license                Print license of the program and exit
      -?,-h,--help             Show this help text
      --type tcp|udp|icmp      connection type
      --host HOSTNAME          host name
      -p,--port PORT           port (ignored for connection type 'icmp')
      --ttl INT                time to live header value
      -t,--timeout FLOAT       connection timeout in seconds
      --trace                  trace routing of the connection
      -p,--print-config        Print the parsed configuration to standard out and
                               exit
      -c,--config-file FILE    Configuration file in YAML format


