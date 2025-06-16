
# CLOSURE GENERATORS -------------------------------------------------------------------------------

#' Definicoes De Geradores De Closure
#' 
#' Funcoes que geram as closures utilizadas para preparo dos regressores em tempo de treino/previsao
#' 
#' Todas as funcoes definidas aqui são closure generators. Isto significa que nao operam no dado
#' diretamente, mas retornam uma nova funcao de um argumento (ou dois, em casos particulares) que
#' realiza o trabalho.
#' 
#' A intencao deste design e garantir que seja possivel replicar exatamente o mesmo pipeline de
#' manipulacao de dados tanto no momento de treino quanto de previsao, simplificando a
#' implementacao de novos modelos e variacoes de regressores, bem como teste e validacao de diversas
#' parametrizacoes.
#' 
#' ## Espeficicacoes
#' 
#' Todas as closures retornadas devem ter um, ou no maximo dois, argumento(s). Estes sao sempre os
#' `data.table`s nos quais a closure opera e serao um dos `data.table`s de dados disponiveis em 
#' tempo de execucao (carga, temperatura obs/prev e feriados).
#' 
#' De forma similar, todas elas devem retornar `data.table`s que seram combinados via `merge` para
#' construcao do dado de regressores finais. Este `merge` sera realizado pela primeira coluna de 
#' cada `data.table`, entao deve ser garantido pelo retorno da closure que esta seja a coluna 
#' correta.
#' 
#' ## Boas praticas
#' 
#' E importante notar que closures carregam seus ambientes onde foram declaradas consigo para que
#' possam encontrar nomes que existiam naquele escopo. Isto e uma caracteristica importante pois
#' e o que nos permite criar instancias de funcoes genericas parametrizadas dinamicamente, porem
#' pode ocasionar problemas de tamanho em memoria/disco apos o salvamento.
#' 
#' Tudo o que nao for necessario para a execucao da closure DEVE SER REMOVIDO DO AMBIENTE antes do
#' retorno.

# --------------------------------------------------------------------------------------------------

#' Gerador De Closure Para Encoder Lagged
#' 
#' Produz uma closure de unico argumento para realizar encoding dwt em dado passado

gen_dwt_builder_lag <- function(var = "", hora_execucao = "07:30:00", L = 64) {

    fun <- function(x) {
        x <- build_lagged_slice(x, var, L, hora_execucao)
        x <- replicate_slice_day_ahead(x)
        x <- as.data.table(x)
        x
    }

    return(fun)
}
