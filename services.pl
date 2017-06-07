%!  service(?Service) is nondet.
%
%   Describe services that are being monitored.  Fields:
%
%     - name:atom
%     Name of the server
%     - scheme:oneof([http,https])
%     Name of the server (default: `http`)
%     - port:integer
%     Port of the server
%     - host:atom
%     Host of the server (default `localhost`)
%     - test:callable
%     Test called.  The test is called as call(Goal, Service, Reply)
%     - timeout:number
%     Max time to wait for the server in seconds.
%     - service:atom
%     Name of the upstart service to restart if the server fails.

service(_{ name:swish,
           scheme:http,
	   port:80,
%	   host:'swish.swi-prolog.org',
	   test:test_pengines,
	   timeout:20,
	   service:swish
	 }).
