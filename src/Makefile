ERLC=erlc
ERL=erl
EBIN=../ebin
INCLUDE=../include
EFLAGS=-I ${INCLUDE} -o ${EBIN}
#SRCS=`ls *.erl`
SRCS=breath_util.erl breath_sup.erl breath_gateway.erl breath_scene.erl breath_master.erl
all:
	  ${ERLC} ${EFLAGS} ${SRCS}
