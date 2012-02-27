

breath_app 模块使用otp里的application模式, 这个模式是整个项目的入口, 实现了两个回调函数(start, stop), stop是在程序结束时被调用, 简单返回ok即可. start函数接受状态参数, 并且调用了breath_sup的start_link函数