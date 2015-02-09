:- module(swimon,
	  [ swimon/0
	  ]).
:- use_module(library(settings)).
:- use_module(library(process)).
:- use_module(library(pengines)).
:- use_module(library(uri)).
:- use_module(library(time)).
:- use_module(library(debug)).

:- setting(interval, number, 300, "Checking interval in seconds").

%:- debug(monitor(restart)).
:- debug(monitor(_)).
:- debug_message_context(+time).

:- initialization swimon.

swimon :-
	monitor_loop.

monitor_loop :-
	repeat,
	catch(monitor, E, print_message(error, E)),
	setting(interval, Time),
	sleep(Time),
	fail.

monitor :-
	forall(service(Service),
	       catch(monitor(Service), E, print_message(error, E))).

monitor(Service) :-
	alive(Service), !.
monitor(Service) :-
	restart(Service).

%%	alive(+Service) is semidet.
%
%	True when Service is alive.

alive(Service) :-
	Test = Service.test,		% FIXME: must be fixed in goal expansion
	(   Timeout = Service.get(timeout)
	->  catch(call_with_time_limit(
		      Timeout,
		      call(Test, Service)), E,
		  ( print_message(error, E), fail))
	;   catch(call(Test, Service), E,
		  ( print_message(error, E), fail))
	),
	debug(monitor(test), '~w: alive', [Service.name]).

restart(Service) :-
	Job = Service.get(service), !,
	debug(monitor(restart), 'Restarting ~w ...', [Service.name]),
	(   debugging(dryrun)
	->  true
	;   process_create('/usr/bin/service', [Job, restart], [])
	).

%%	service(?Service) is nondet.
%
%	Describe services that are being monitored.

service(_{ name:swish,
	   port:80,
%	   host:'swish.swi-prolog.org',
	   test:test_pengines,
	   timeout:20,
	   service:swish
	 }).


%%	uri_dict(+URI, +Dict) :-

uri_dict(URI, Dict) :-
	var(URI),
	_{scheme:Scheme, authority:Authority,
	  path:Path, search:Search, fragment:Fragment} >:< Dict,
	fill_authority(Authority, Dict),
	fill_search(Search, Dict),
	uri_components(URI, uri_components(Scheme, Authority,
					   Path, Search, Fragment)).

fill_authority(Authority, Dict) :-
	var(Authority), !,
	_{user:User,password:Password,host:Host,port:Port} >:< Dict,
	uri_authority_components(Authority,
				 uri_authority(User, Password, Host, Port)).
fill_authority(_, _).

fill_search(Search, Dict) :-
	var(Search),
	uri_query_components(Search, Dict.get(query)), !.
fill_search(_, _).

%%	service_url(+Service, -URL) is det.
%
%	URL is the URL at which Service lives.

service_url(Service, URL) :-
	uri_dict(URL, _{scheme:http,host:localhost}.put(Service)).


		 /*******************************
		 *      SPECIFIC SERVICES	*
		 *******************************/
:- public
	test_pengines/1.

test_pengines(Service) :-
	service_url(Service, URL),
	pengine_rpc(URL, current_prolog_flag(version, V)),
	integer(V).

