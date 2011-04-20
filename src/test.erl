%%在Eshell下声明record users
1> rd(users, {uid,website="t.qq.com/lajabs",name,time=time()}).
users
%%对ets建表,并创建以record元素uid作为索引键,以下的增删改查都将使用它
2> ets:new(users,[public,set,named_table,{keypos, #users.uid}]).
users
%%创建record,并对元素赋值
3> Users=#users{uid=101,name=lajabs}.
#users{uid = 101,website="t.qq.com/lajabs",name = lajabs,
  time = {16,48,32}}
%%将创建好的record存入ets
4> ets:insert(users,Users).
true
%%创建第二个record
5> Users2=#users{uid=102,name=laja2}.
#users{uid = 102,website="t.qq.com/lajabs",name = laja2,
  time = {16,49,16}}
%%把record users也存入ets,这时users表中有2笔记录
6> ets:insert(users,Users2).        
true
%%尝试查询uid(索引键)为102的记录,成功返回record(之前定义的变量Users2),需要注意返回的是列表(带[])
7> ets:lookup(users,102).
[#users{uid = 102,website="t.qq.com/lajabs",name = laja2,
    time = {16,49,16}}]
%%对ets更新record,将元素name改为'hello'
8> ets:update_element(users,102,{#users.name, hello}).
true
%%查看修改后的结果,发现已经更改成功
9> ets:lookup(users,102).                            
[#users{uid = 102,website="t.qq.com/lajabs",name = hello,
    time = {16,49,16}}]
%%删除uid(索引键)为102的记录
10> ets:delete(users,102).
true
%%查看结果,已删除成功,返回的是空列表[]
11> ets:lookup(users,102).
[]
%%查询记录中的某个元素值可以用以下方式
12> ets:lookup_element(users,101,#users.name).
lajabs 
