FROM sbtscala/scala-sbt:openjdk-17.0.2_1.7.1_3.2.0

RUN git clone https://github.com/HaoxianChen/declarative-smart-contracts.git
WORKDIR declarative-smart-contracts
RUN git checkout verification

COPY libz3.so ./
COPY libz3java.so ./
COPY z3-4.8.14/bin/com.microsoft.z3.jar declarative-smart-contracts/unmanaged

ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/root/declarative-smart-contracts

RUN sbt compile

COPY project/plugins.sbt project/
RUN sbt assembly

COPY check-all.py ./
