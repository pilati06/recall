# Código de execução

## Execução simples

cargo run --release .\src\assets\contracts\the_matrix.rcl

## Exemplo de execução com argumentos

cargo run --release .\src\assets\contracts\the_matrix.rcl -vg

## Argumentos possíveis

```
OPTIONS:
    -h, --help          Print this message and exit
    -v, --verbose       Turn on the verbose mode
    -g                  Exports the automaton into a graphviz file
                        Default filename is <CONTRACT_FILE>.dot
    -n, --no-prunning   Don't use the prunning method
    -c, --continue      Continues the analysis if a conflict is found
    -m                  Export minimized automaton
```
