

breath_sup模式使用otp中的supervisor模式, 暴露给模块start_link和init函数, init是在模块初始化的时候由erlang调用, 这里使用了一个常用的编码模式, 定义一个child_spec函数, 他会调用后面模块的child_spec函数, 返回值交给Controller
init做的另一件事情是定义一些项目的配置以及定义需要监控的模块, 这里我们定义服务器的监听端口和地址 0.0.0.0:2223
定义breath监控的模块: breath_server,breath_mod, 参数是permanent, 代表永远运行. 然后在命令行下打印process