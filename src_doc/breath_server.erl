

breath_server模块是我们项目实际的运行主模块, 被supervisor监控, 下面会声明一些需要用到的子模块. 对外暴露start和stop函数, start调用了一个prepare函数, 里面声明了需要用到的一些系统模块, 这里用到了crypo, public_key, inets, ssl

crypto是用来加密协议的模块, erlang官方文档有介绍: http://www.erlang.org/doc/apps/crypto/index.html, 包括了一些常用加密算法的加密(md5, sha, hmac, rsa, dss等)

public_key是按照RFC5280标准erlang的接口实现, erlang.org上的文档地址:http://www.erlang.org/doc/man/public_key.html, 可以让远程通信使用public_key作为安全策略

inets是erlang的网络模块, erlang.org上的文档地址:http://www.erlang.org/doc/apps/inets/index.html, 包括基础服务, ftp, tftp, httpc, httpd, socket, 等模块, 以及相关的安全模块