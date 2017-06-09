/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(swiguard,
	  [ swiguard/0
	  ]).
:- use_module(library(settings)).
:- use_module(library(process)).
:- use_module(library(pengines)).
:- use_module(library(uri)).
:- use_module(library(time)).
:- use_module(library(debug)).

:- setting(interval, number, 300, "Checking interval in seconds").

%:- debug(guard(restart)).
:- debug(guard(_)).
:- debug_message_context(+time('%D %T.%3f')).

:- initialization(swiguard, main).

defaults(_{ scheme:http,		% URI scheme
	    host:localhost,		% Service host
	    retry:1			% # Retries
	  }).

swiguard :-
	guard_loop.

guard_loop :-
	repeat,
	catch(guard, E, print_message(error, E)),
	setting(interval, Time),
	sleep(Time),
	fail.

guard :-
	defaults(Defaults),
	forall(user:service(Service),
	       catch(guard(Defaults.put(Service)), E, print_message(error, E))).

guard(Service) :-
	between(0, Service.retry, _),
	alive(Service), !.
guard(Service) :-
	ignore(catch(stacks(Service), E, print_message(error, E))),
	restart(Service).

%%	alive(+Service) is semidet.
%
%	True when Service is alive.

alive(Service) :-
	Test = Service.test,		% FIXME: must be fixed in goal expansion
	get_time(T0),
	(   Timeout = Service.get(timeout)
	->  catch(call_with_time_limit(
		      Timeout,
		      call(Test, Service, Reply)), E,
		  ( print_message(error, E), fail))
	;   catch(call(Test, Service, Reply), E,
		  ( print_message(error, E), fail))
	),
	get_time(T1),
	T is T1-T0,
	debug(guard(test), '~w: alive (~3f sec; ~p)', [Service.name, T, Reply]).

restart(Service) :-
	Job = Service.get(service), !,
	debug(guard(restart), 'Restarting ~w ...', [Service.name]),
	(   debugging(dryrun)
	->  true
	;   process_create('/usr/bin/service', [Job, restart], [])
	).

stacks(Service) :-
	Job = Service.get(service),
	SWIPLBT = Service.get(backtrace),
	service_pid(Job, PID), !,
	format(atom(OutOption), '--out=~w.~w.stack', [Job, PID]),
	process_create(SWIPLBT,
		       [ '--thread=all', '-C', OutOption, PID ],
		       []).
stacks(_).

service_pid(Job, PID) :-
	setup_call_cleanup(
	    process_create('/usr/bin/service', [Job, status],
			   [ stdout(pipe(Out))
			   ]),
	    read_string(Out, _, Data),
	    close(Out)),
	split_string(Data, " ", " ", List),
	append(_, [process,PIDS|_], List),
	number_string(PID, PIDS).


%!  service_url(+Dict, -URI) is det.
%
%   URL is the URL at which Service lives.

service_url(Dict, URI) :-
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


		 /*******************************
		 *      SPECIFIC SERVICES	*
		 *******************************/
:- public
	test_pengines/2.

test_pengines(Service, V) :-
	service_url(Service, URL),
	pengine_rpc(URL, statistics(threads, V)),
	integer(V).

