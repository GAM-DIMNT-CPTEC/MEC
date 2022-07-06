# MEC

_Model Evaluation Comparator_ (MEC).

## Obtenção 

Para realizar uma cópia do MEC, utilize o comando a seguir, ou clique no botão `Code > Download Zip`:

```
gh repo clone GAM-DIMNT-CPTEC/MEC
```

## Pré-requisitos

O MEC utiliza pacotes diferentes para a etapa de pré-processamento, cálculo das estatísticas e plotagem das figuras. Para a instalação destes pacotes, recomenda-se a utilização do Anaconda para a criação de um ambiente específico para o MEC.

Com o Anaconda instalado na máquina, utilize o arquivo `MEC.yml` para criar o ambiente:

```
conda env create -f MEC.yml
```

Além dos pacotes do R instalados no ambiente `MEC-test` (criado na etapa anterior), é necessário instalar também o pacote `dashboardthemes`. Para isso, siga as instruções a seguir:

1. Ative o ambiente `MEC-test`:
  
    ```
    conda activate MEC-test
    ```

    **Nota:** Caso o MEC esteja sendo executado dentro da máquina Itepemirim, o ambiente `MEC-test` poderá ser ativado com o comando `source /caminho/instalacao/do/conda/envs/MEC-test/bin/activate`.


2. Abra o prompt do `R` e instale o pacote:

    ```
    R
    install.packages("dashboardthemes")
    ```

3. Selecione o mirror mais próximo da sua localização geográfica e aguarde o processo terminar.

## Configuração

Antes de iniciar o processo de configuração do MEC, ative o ambiente `MEC-test` criado na etapa anterior:

```
conda activate MEC-test
```

Para configurar o MEC, siga as instruções a seguir.

1. Dentro do diretório `MEC`, criar um diretótio para o experimento, e.g., `REG21T-test` ou copiar um dos diretórios `REG21`, `REG21T` ou `REG21UVP`:

    ```
    cd MEC
    mkdir REG21T-test
    ```

    ou

    ```
    cp -R REG21T REG21T-test
    ```

2. No diretório `MEC/REG21T-test`, verificar os nomes dos experimentos a serem avaliados e os nomes dos experimentos de referência e das datas de avaliação no script `DefEvalCfg.ksh`:

    * `MEC_models`: nomes dos experimentos a serem avaliados (e.g., `BAM_20km`, `BRAMS_08km`, `Eta_08km`, `GFS_30km`, `WRF_07km`, `WRFG_07km`);
    * `MEC_models_cmp`: nome dos experimentos a serem considerados como referência (e.g., `BAM_20km`, `BRAMS_08km`, `Eta_08km`, `WRF_07km`);
    * `MEC_grid_res`: arquivo `ctl` com os parâmetros da grade a ser utilizada como padrão (verificar o diretório `TEMPLATE`);

3. Ainda no diretório `MEC/REG21T-test`, revise os seguintes parâmetros do script `00-EvalSetInit_run_once.ksh`:

    * `MEC_eval_name`: nome do experimento, no exemplo `REG21T-test`;
    * `MEC_ref_per`: período da avaliação no formato `YYYYMM`;
    * `MED_D_data`: local onde serão escritos os arquivos dos modelos e observações na etapa de pré-processamento, no exemplo `/algum/local/MEC_DATA`.

4. Executar o script `00-EvalSetInit_run_once.ksh`. Este script criará o diretório `/algum/local/MEC_DATA` e copiará os arquivos necessários para a etapa de pré-processamento do MEC:

    ```
    ./00-EvalSetInit_run_once.ksh
    ```

5. Ainda no diretório `MEC/REG21T-test`, revise os scripts `01-CFG_Main.R`, `03-CFG_SET_TMAX_SAMeT.R` e `04-CFG_SET_TMIN_SAMeT.R`. Observe que os scripts `03-CFG_SET_TMAX_SAMeT.R` e `04-CFG_SET_TMIN_SAMeT.R` referem-se à variável que será avaliada. Execute os scripts utilizando os comandos:

    ```
    Rscript 01-CFG_Main.R
    Rscript 04-CFG_SET_TMIN_SAMeT.R
    ```

    e também o script `03-CFG_SET_TMAX_SAMeT.R`, caso a variável `TMAX` também seja considerada na avaliação:

    ```
    Rscript 03-CFG_SET_TMAX_SAMeT.R
    ```

6. No diretório `MEC/CONFIG`, revisar os caminhos e as datas de avaliação dos modelos (e.g., o script `BAM_20km.ksh`, caso apenas o modelo BAM 20 km seja considerado na avaliação). Revisar os seguintes parâmetros:

    * `MEC_ctl_full_path_model`: diretório onde se encontram os arquivos brutos do modelo (e.g., `/oper/share/ioper/tempo/BAM/TQ0666L064/brutos/YYYY/MM/DD/00` para o modelo BAM 20 km);
    * `MEC_ctl_full_fname_model`: nome completo do arquivo `ctl` template do modelo (e.g., `${MEC_ctl_full_path_model}/GPOSNMCYYYYMMDD00P.fct.TQ0666L064.ctl` para o modelo BAM 20 km).

7. No diretório `MEC/`, revise os diretórios do script `03-Eval.ksh`.

## Uso

Execute o script `00-MEC.ksh` sem parâmetros para visualizar as opções de execução do script:

```
./00-MEC.ksh

Use:

./00-MEC.ksh <eval_name> <var> <init_time> <valid_date1> <valid_date2> <regrid> <sp_domain>

<eval_name>   A name for the evaluation, root directory of the main output directory, e.g. REG21 *** REQUIRED ***
<var>         Variable name (appears on the GUI/plots), e.g. PREC | TP2M | ...
<init_time>   Model's initialization time, e.g. 00 | 12
<valid date1> Initial valid date, target of the forecast, in format YYYYMMDD
<valid date2> Ending valid date, target of the forecast, in format YYYYMMDD
<regrid>      Regridding mode:
              . LOWEST       = Automatically chooses the lowest res grid between model x obs
              . CTL filename = uses the CTL as regriding target, e.g. BAM_20km.ctl
<sp_domain>   Spatial domain:
              . MCD = Max Common Domain, auto gets the max area between model x obs
              . LON1_LON2_LAT1_LAT2, e.g. 270_30_-50_10

or

./00-MEC.ksh <EVAL_NAME>

<EVAL_NAME>   A name for the evaluation, root directory of the main output directory, e.g. REG21 *** REQUIRED ***

The system will run <eval_name>/DefEvalCfg.ksh to setup the remaining env variables.
```

Como exemplo, para relizar o MEC, para uma avaliação curta (para o período de 20220601 a 20220605, às 00Z) da tempertura mínima (TMIN) do modelo BAM 20 km (configurado seguindo as instruções das seções anteriores), considerando o máximo domínio comum (MCD), utilize o comando a seguir:

```
./00-MEC.ksh REG21T-test TMIN 00 20220601 20220605 MERGE_10.ctl MCD
```

Ao final da execução do script `00-MEC.ksh`, serão criados os arquivos binários prontos para a plotagem dos resultados das avaliações.

Na listagem a seguir, constam os arquivos que serão criados e que serão utilizados nas próximas etapas (observe que o diretório `/algum/local/MEC_DATA/REG21T-test` é o local escolhido para armazenar os resultados do MEC):

```
ls -ltr /algum/local/MEC_DATA/REG21T-test/DEPLOY/
total 140K
-rw-rw-r-- 1 user group  269 Jun 29 12:46 DIR.RData
-rw-rw-r-- 1 user group  171 Jun 29 12:46 METRICS.RData
-rw-rw-r-- 1 user group  219 Jun 29 12:46 REGIONS.RData
-rw-rw-r-- 1 user group  231 Jun 29 12:46 THRESHOLDS.RData
-rw-rw-r-- 1 user group 3.5K Jul  5 12:15 CFG.RData
-rw-rw-r-- 1 user group 2.0K Jul  5 12:15 OBS_TMIN_SAMeT.RData
-rw-rw-r-- 1 user group 3.9K Jul  5 12:15 EVAL_CONT_1_00.RData
-rw-rw-r-- 1 user group 3.8K Jul  5 12:15 EVAL_CONT_2_00.RData
-rw-rw-r-- 1 user group 3.8K Jul  5 12:15 EVAL_CONT_3_00.RData
-rw-rw-r-- 1 user group 3.9K Jul  5 12:15 EVAL_CONT_4_00.RData
-rw-rw-r-- 1 user group 3.9K Jul  5 12:15 EVAL_CONT_5_00.RData
-rw-rw-r-- 1 user group 3.9K Jul  5 12:15 EVAL_CONT_6_00.RData
-rw-rw-r-- 1 user group 3.9K Jul  5 12:15 EVAL_CONT_7_00.RData
-rw-rw-r-- 1 user group 3.9K Jul  5 12:15 EVAL_CONT_8_00.RData
-rw-rw-r-- 1 user group 3.9K Jul  5 12:15 EVAL_CONT_9_00.RData
-rw-rw-r-- 1 user group 3.9K Jul  5 12:15 EVAL_CONT_10_00.RData
```

## Visualização

A partir da utilização do script `00-MEC.ksh`, as etapas de pré-processamento e cálculo das estatísticas são realizadas. A visualização dos resultados é realizada com a utilização da interface gráfica do MEC, a qual pode ser invocada a partir da linha de comando. Para utilizar a interface gráfica do MEC, será necessário realizar os seguintes passos.

1. Entre no diretório `MEC/MECInterface`, edite o arquivo `AUX-CURRENT_EVAL.txt` com o nome do experimento realizado: `REG21T-test`;
2. Ainda no diretório `MEC/MECInterface`, crie um diretório com o nome do experimento `REG21T-test`;
3. Copie o conteúdo do diretório `/algum/local/MEC_DATA/REG21T-test/DEPLOY/` para dentro do diretório criado no passo anterior (`MEC/MECInterface/REG21T-test/`);
4. Execute o script `runMECInterface.R` dentro do diretório `MEC/MECInterface`:

    ```
    Rscript runMECInterface.R
    ```
