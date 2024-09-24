#' homepage of UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @noRd

## Part1.1 home page ---------------------------------------------------------------

homepage_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    title = 'Home page',
    icon = bs_icon('house'),

    # 使用fluidRow和column来组织内容，增加间距和视觉层次
    fluidRow(

      # # logo和欢迎信息区域
      # column(
      #   width = 4, offset = 4,
      #   style = "margin-top: 20px;",
      #   align="center",
      #   tags$div(
      #     tags$img(src = "www/MetMiner.jpg", style = "max-width: 85%; height: auto;")
      #
      #   )
      # ),
      #
      # # 描述文本区域
      # column(
      #   width = 8, offset = 2,
      #   style = "border: 2px dashed rgba(0, 128, 128, 0.5); border-radius: 10px; padding: 20px;",
      #   align="justify",
      #   tags$style(type="text/css", "
      #     body { font-size: 16px; line-height: 1.5; }
      #     footer { font-size: 12px; }
      #   "),
      #   HTML(
      #     paste(
      #       "<h2>Introduction</h2>",
      #       "<hr style='border-top: 6px double #008080; border-bottom: 3px solid #008080;'>",
      #       "<p><strong style='color: #008080;font-size: larger;'>MetMiner</strong> is a <strong style='color: blue;'>user-friendly</strong> R-Shiny-based workflow designed for processing and mining non-targeted metabolomics data. Tailored for wet-lab biologists, it enables rapid mastery of non-targeted metabolomic analysis and can be deployed on servers or clusters to process <strong style='color: blue;'>large-scale metabolomics data</strong>.</p>",
      #       "<p>The integration of the <a href='https://github.com/tidymass' target='_blank'><strong style='color: blue;'>TidyMass project<sup id='ref1'>1</sup></strong></a> introduces the mass_dataset class, an advanced mass spectrometry data management format that supports flexible data input and output. This format accommodates both raw data and peak-picked results from other software, and it meticulously logs data cleaning and metabolite annotation to ensure reproducibility, traceability, and transparency.</p>",
      #       "<p>We have developed the <a href='https://github.com/ShawnWx2019/MDAtoolkits' target='_blank'><strong style='color: blue;'>Metabolomics Downstream Analysis toolkits (MDAtoolkits)</strong></a>, which assist in metabolite classification, multivariate and univariate statistical analyses, and enrichment analysis. We also provide a collection of plant-specific metabolic databases, optimized for plant metabolomics annotations.</p>",
      #       "<p>The MetMiner shiny app features an array of <strong style='color: blue;'>graphical interactive operations</strong> that enable various data linkages, allowing users to engage deeply with data analysis and mining processes.</p>",
      #       "<p>The metMiner shiny app has been packaged into a TBtools<sup id='ref1'>2</sup> plugin, which can be downloaded and installed through the <strong style='color: blue;'>TBtools plugin store</strong>. Thanks to TBtools for providing a convenient dependency resolution solution. </p>"
      #     )
      #   ),
      #
      #   # 图片区域
      #   tags$div(
      #     align="center",
      #     tags$img(src = "www/Fig1.StructureAndStrategy.webp", style = "max-width: 80%; height: auto;")
      #   ),
      #
      #   # 说明书
      #   HTML(
      #     paste(
      #       "<h2>Cookbook</h2>",
      #       "<hr style='border-top: 6px double #008080; border-bottom: 3px solid #008080;'>",
      #       "Step by step users manual:<a href='https://shawnwx2019.github.io/metminer-cookbook/' target='_blank'> https://shawnwx2019.github.io/metminer-cookbook/</a></p>"
      #     )
      #   ),
      #
      #   # 引用信息区域
      #   HTML(
      #     paste(
      #       "<h2>How to cite</h2>",
      #       "<hr style='border-top: 6px double #008080; border-bottom: 3px solid #008080;'>",
      #       "<p>If you have used this app for metabolomics analysis in your publication, please cite the following papers:</p>",
      #       "<ul>",
      #       "<li>MetMiner: A user-friendly pipeline for large-scale plant metabolomics data</li>",
      #       "<li>TidyMass: Shen X, et al. (2022). TidyMass an object-oriented reproducible analysis framework for LC-MS data. Nat Commun. 13(1):4365. <a href='https://www.nature.com/articles/s41467-022-32155-w' target='_blank'>doi:10.1038/s41467-022-32155-w.</a></li>",
      #       "<p>If you installed metMiner from TBtools plugin store, please cite:</p>",
      #       "<li>TBtools: Chen, et al. (2023). TBtools-II: 'A one for all, all for one' bioinformatics platform for biological big-data mining. Molecular Plant 16(11): 1733-1742. <a href='10.1016/j.molp.2023.09.010' target='_blank'>doi:10.1016/j.molp.2023.09.010</a></li>",
      #       "</ul>",
      #       "<p>Thank you!</p>"
      #     )
      #   ),
      #   # 参考文献
      #   HTML(
      #     paste(
      #       "<h2>Reference</h2>",
      #       "<hr style='border-top: 6px double #008080; border-bottom: 3px solid #008080;'>",
      #       "<p><sup>1</sup>TidyMass: Shen X, et al. (2022). TidyMass an object-oriented reproducible analysis framework for LC-MS data. Nat Commun. 13(1):4365. <a href='https://www.nature.com/articles/s41467-022-32155-w' target='_blank'>doi:10.1038/s41467-022-32155-w.</a></p>",
      #       "<p><sup>2</sup>TBtools: Chen, et al. (2023). TBtools-II: 'A one for all, all for one' bioinformatics platform for biological big-data mining. Molecular Plant 16(11): 1733-1742. <a href='10.1016/j.molp.2023.09.010' target='_blank'>doi:10.1016/j.molp.2023.09.010</a></p>"
      #
      #     )
      #   ),
      # )
    ),

    # 添加Footer
    hr_head(),
    tags$footer(style="text-align:center; margin-top: 20px;",
                "")
  )
}

