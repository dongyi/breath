ERLC=erlc
ERL=erl
EBIN=../ebin
INCLUDE=../include
EFLAGS=-I ${INCLUDE} -o ${EBIN}
#SRCS=`ls *.erl`
SRCS=breath_gateway.erl breath_web.erl breath_room.erl breath_postoffice.erl 
all:
	  ${ERLC} ${EFLAGS} ${SRCS}
