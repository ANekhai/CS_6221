#     # #Docker file for helloscotty
#     # FROM fpco/stack-build:lts-15.16
#     # MAINTAINER Adi Maini <adimaini@vt.edu>
#     # ADD static static
#     # ADD static src 
#     # EXPOSE 5000
#     # ENTRYPOINT ./image-exe

# FROM ubuntu:16.04
# RUN mkdir -p \opt\myapp/
# ARG BINARY_PATH
# WORKDIR /opt/myapp
# RUN apt-get update && apt-get install -y \
#   ca-certificates \
#   libgmp-dev
# COPY "$BINARY_PATH" /opt/myapp
# COPY src \opt\myapp\src
# COPY app /opt\myapp\app
# EXPOSE 5000
# CMD ["/opt/myapp/image-exe"]

# FROM fpco/stack-build:lts-15.16 as build
# RUN mkdir /opt/build
# COPY . /opt/build
# RUN cd /opt/build && stack build --system-ghc

# RUN mkdir -p /opt/myapp
# ARG BINARY_PATH
# WORKDIR /opt/myapp
# RUN apt-get update && apt-get install -y \
#   ca-certificates \
#   libgmp-dev
# # NOTICE THIS LINE
# COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-15.16/8.0.2/bin .
# COPY app /opt/myapp/
# COPY src /opt/myapp/src
# EXPOSE 5000
# CMD ["/opt/myapp/image-exe"]


# FROM fpco/stack-build:lts-15.16 as build
# ADD . . 

# FROM ubuntu:16.04
# COPY --from=build /Users/adimaini/Documents/GW/Advanced Software Paradigms/Project/CS_6221.nosync/.stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/219c4eef637de81431e864021af4dfc5a76fc6002e563868e1dfc24dcc774697/8.8.3/bin .
# EXPOSE 5000
# CMD ["image-exe"]

FROM fpco/stack-build:lts-15.16
ADD src src
ADD app app
FROM ubuntu:latest
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev 
ADD .stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/219c4eef637de81431e864021af4dfc5a76fc6002e563868e1dfc24dcc774697/8.8.3/bin /bin/
EXPOSE 5000
ENTRYPOINT [ "/bin/image-exe" ]