FROM ubuntu:16.10

RUN apt-get update -y && apt-get full-upgrade -y
RUN apt-get install -y bash-completion zsh curl cowsay git vim bsdmainutils

ADD bash_completion.sh /usr/bin/bash_completion.sh
ADD _bash_completion.sh /usr/local/share/zsh/site-functions/_bash_completion.sh
ADD bash_completion.sh.bash-completion /etc/bash-completion.d/bash_completion.sh
RUN chmod 755 /usr/bin/bash_completion.sh

#
# Install oh-my-zsh
#
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)" || true

#
# Enable bash completion
#
RUN echo '\n\
. /etc/bash_completion\n\
source /etc/bash-completion.d/bash_completion.sh\n\
' >> ~/.bashrc

#
# Setup prompt
#
RUN echo 'export PS1="[Swagger Petstore] \$ "' >> ~/.bashrc
RUN echo 'export PROMPT="[Swagger Petstore] \$ "' >> ~/.zshrc

#
# Setup a welcome message with basic instruction
#
RUN echo 'cat << EOF\n\
\n\
This Docker provides preconfigured environment for running the command\n\
line REST client for $(tput setaf 6)Swagger Petstore$(tput sgr0).\n\
\n\
For convenience, you can export the following environment variables:\n\
\n\


\n\
$(tput setaf 7)Basic usage:$(tput sgr0)\n\
\n\
$(tput setaf 3)Print the list of operations available on the service$(tput sgr0)\n\
$ bash_completion.sh -h\n\
\n\
$(tput setaf 3)Print the service description$(tput sgr0)\n\
$ bash_completion.sh --about\n\
\n\
$(tput setaf 3)Print detailed information about specific operation$(tput sgr0)\n\
$ bash_completion.sh <operationId> -h\n\
\n\
By default you are logged into Zsh with full autocompletion for your REST API,\n\
but you can switch to Bash, where basic autocompletion is also supported.\n\
\n\
EOF\n\
' | tee -a ~/.bashrc ~/.zshrc

ENTRYPOINT ["zsh"]
