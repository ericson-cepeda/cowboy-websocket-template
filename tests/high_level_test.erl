-module(high_level_test).
-export([run/0]).

run() ->
    io:format("\nIntial s_group config:\n~p\n", [application:get_env(kernel, s_groups)]),
    io:get_chars("Press any key to continue ...\n", 1),
    io:format("Running orbit test\n"),
    timer:sleep(1000),
    orbit:run_on_five_nodes(),
    io:get_chars("Press any key to continue ...\n", 1),
    io:format("Delete group1...\n"),
    timer:sleep(1000),
    Res1=s_group:delete_s_group(group1),
    io:format("delete_s_group result:~p\n", [Res1]),
    io:get_chars("Press any key to continue ...\n", 1),
    io:format("Create new s_group group1 consisting node1 and node2 ...\n"),
    timer:sleep(1000),
    Res2=s_group:new_s_group(group1, ['node1@127.0.0.1', 'node2@127.0.0.1']),
    io:format("New s_group result:~p\n", [Res2]),
    io:get_chars("Press any key to continue ...\n", 1),
    io:format("Running Orbit on group one only...\n"),
    timer:sleep(1000),
    Nodes1 = ['node1@127.0.0.1','node2@127.0.0.1'],
    bench:dist_seq(fun bench:g124/1, 100000,8,Nodes1),
    io:get_chars("Press any key to continue ...\n", 1),
    io:format("Add node3 to group1...\n"),
    timer:sleep(1000),
    Res3=s_group:add_nodes(group1, ['node3@127.0.0.1']),
    io:format("Add nodes to group1 result:~p\n", [Res3]),
    io:get_chars("Press any key to continue ...\n", 1),
    io:format("Running orbit on group 2 only...\n"),
    timer:sleep(1000),
    Nodes2 = ['node3@127.0.0.1','node4@127.0.0.1', 'node5@127.0.0.1'],
    bench:dist_seq(fun bench:g124/1, 100000,8,Nodes2),
    io:get_chars("Press any key to continue ...\n", 1),
    io:format("Remove node3 from group1...\n"),
    timer:sleep(1000),
    Res4=s_group:remove_nodes(group1, ['node3@127.0.0.1']),
    io:format("remove nodes from group1 result:~p\n", [Res4]),
    io:get_chars("Press any key to continue ...\n", 1),    
    io:format("Re-adding node3 to group1...\n"),
    timer:sleep(1000),
    Res5=s_group:add_nodes(group1,['node3@127.0.0.1']),
    io:format("Add nodes to group1 result:~p\n",[Res5]),
    ok.