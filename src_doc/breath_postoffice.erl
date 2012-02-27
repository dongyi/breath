breath_postoffice是一个典型的otp gen_server模式
需要提供给项目其他模块init, handle_call, handle_cast, terminate, code_change, handle_info这几个标准gen_server回调函数.
另外提供了get/2和init/3
定义一些常量, 比如TCP_SEND_TIMEOUT设为15000毫秒
breath_postoffice作用上顾名思义是一个消息中间件, 类似消息队列, 客户端发送过来的消息先存在这里, 等分发进程去把消息发送到应该发送的客户端
我们先要定义一个存储结构, 使用了record, 由于我们所有的消息都不需要持久化, 所以一直在内存中, 只要使用record即可, 如果需要持久化, erlang推荐的做法是用mnesia, 一个持久化的key-value数据库

record的名字是breath_postoffice, 成员有port, loop, name, ip, listen, nodelay, backlog, active_sockets, acceptor_pool_size, ssl, ssl_opts, acceptor_pool
record实际上是一个数组, 我们用标签来给他们的数组index做标记, start函数调用后模块开始执行, 首先使用了一个State的变量保存breath_postoffice, (注意本文中保存一词仅仅代表保存在内存中) State=#breath_postoffice{}.

后面parse_options函数用来解析选项, 这是一个内部函数. 给这个结构设置成员值, 比如parse_options([{port, Port} | Rest], State) ->
    parse_options(Rest, State#breath_postoffice{port=Port});这个分支
    接受的参数是一个列表和一个State, 列表的第一个元素是{port, Port}, 这里表示定义端口的值, 把State状态的port设为Port参数的值, 然后继续处理别的分支. 最后的结束条件
    parse_options([], State) ->
      State;
    全部解析完成后返回State
start函数接受一个Option值, 里面包括各种选项的值, 经过parse_options解析, 就把record里的各个成员赋值, 然后调用start_server

这个模块中暴露的get方法是一个gen_server:call的封装, 调用get的参数和属性

start_server函数首先判断State里ssl和name, 如果ssh选项值为true, 需要保证crypto, public_key, ssl三个模块启动
如果Name未定义, 调用gen_server的start_link, 不加任何参数, 否则调用模块内的Name函数

其他的一些函数: ipv6_supported, 使用inet的getaddr, 判断localhost有没有inet6地址, 如果有返回true, 如果没有返回false
模块的init函数, 接收State结构, 其中参数有Ip, Port, Backlog, NoDelay, 首先设置trap_exit为true, 设置捕获EXIT信号为真来改变默认行为
然后定义一些基本选项, 重用地址true, 数据包为4个字节, send_timeout为前面定义的TCP_SEND_TIMEOUT值, keepalive为true(在http1.1标准里默认的http连接都是keepalive的, 在http请求头的connection:keep-alive定义, 如果不需要长连接支持, 需要强制设置connection:close), 另外设置active为false, nodelay
设置好状态之后, 调用了listen函数, 这里的listen并非标准tcp协议里的listen, 而是调用了breath_gateway里的listen, 所有网络底层代码都是由breath_gateway来处理的, 后面会详细介绍 
breath_gateway:listen返回ok, Listen后, 继续调用breath_gateway的port获取已经监听端口的句柄, 并且建立一个acceptor_pool
acceptor_pool是一个连接池, 存放所有连接的pid, 自动管理重用, 
