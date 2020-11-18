# daemon-test

Just playing around with daemons library for haskell.

Some notes:

 - the daemons dependency is a github version from [msrdic/daemons](https://github.com/msrdic/daemons) because [`kill` signal wasn't behaving as expected](https://github.com/msrdic/daemons/commit/3cb08ef558882e795a171224cd949e9e93a1e4cf).