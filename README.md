# MEC

_Model Evaluation Comparator_ (MEC).

## Obtenção 

Para realizar uma cópia do MEC, utilize o comando a seguir, ou clique no botão `Code > Download Zip`:

```
gh repo clone GAM-DIMNT-CPTEC/MEC
```

## Configuração

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
3. Ainda no diretório `MEC/REG21T-test`, revisar os seguintes parâmetros do script `00-EvalSetInit_run_once.ksh`:
* `MEC_eval_name`: nome do experimento, no exemplo `REG21T-test`;
* `MEC_ref_per`: período da avaliação no formato `YYYYMM`;
* `MED_D_data`: local onde serão escritos os arquivos dos modelos e observações na etapa de pré-processamento, no exemplo `/algum/local/MEC_DATA`.
4. Executar o script `00-EvalSetInit_run_once.ksh`. Este script criará o diretório `/algum/local/MEC_DATA` e copiará os arquivos necessários para a etapa de pré-processamento do MEC:

```
./00-EvalSetInit_run_once.ksh
```

5. No diretório `MEC/CONFIG`, revisar os caminhos e as datas de avaliação dos modelos (e.g., o script `BAM_20km.ksh`, caso apenas o modelo BAM 20 km seja considerado na avaliação). Revisar os seguintes parâmetros:
* `MEC_ctl_full_path_model`: diretório onde se encontram os arquivos brutos do modelo (e.g., `/oper/share/ioper/tempo/BAM/TQ0666L064/brutos/YYYY/MM/DD/00` para o modelo BAM 20 km);
* `MEC_ctl_full_fname_model`: nome completo do arquivo `ctl` template do modelo (e.g., `${MEC_ctl_full_path_model}/GPOSNMCYYYYMMDD00P.fct.TQ0666L064.ctl` para o modelo BAM 20 km).

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

Como exemplo, para relizar o MEC, para uma avaliação curta (apenas para o dia 20220601 às 00Z) da tempertura mínima (TMIN) do modelo BAM 20 km (configurado seguindo as instruções das seções anteriores), considerando o máximo domínio comum (MCD), utilize o comando a seguir:

```
./00-MEC.ksh REG21T-test TMIN 00 20220601 20220601 LOWEST MCD
```
