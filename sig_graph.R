## DESCRIPTION:
## MAKES THE TRGRESSION STATISTICAL TEST OF TWO GROUP OF VARIABLES IN A DATAGRAME.
## THE FIRST VARTIABLE CONTAINS THE RESPONSES THE SECOND CONTAINS THE PRECCTORS IN THE LINEAR REGRESSION ANALYS.
## THE SIGNIFICAT VALUE THERSH HOLD AND TTHE RSUWARE CAN BE SPECIFIED.
## USAGE"
## USE TH MAIN_SIGNIFICANT_NESTED_MAPPED_LM_AND_PLOTS WITH ARGUMENTS:
## DATASET, PREDICTORS, RESPONSES, SIGNIFICANT THRESHOW, MIN R SQUARED ETC.
## RETURN OUTPUT:
## A LIST NAMED:

## significant_analyses_list

## IT ALSO SAVES TO DISK (AS Z.PNG) THE A GRID OF PLOTS CONTAINING THE SCATERPLOTS OF THE SIGNIFICANT ANALUSES.
## THE 

## regression_significant_main

## FUNCTION RETURNS A LIST WITN FOUR ELEMENTS:
## A LIST CONTAINING THE PARAMETER OF THE SIGNIFICANT ANALYSES,
## A MATRIX CONTAININT THE SIGNIFICANT PVALUES FROM THE LM ANALYSES,
## THE GRID OF PLOTS, AND, THE LIST OF THE INDIVIDUAL SCATER PLOTS.

## INSTALL LIBRARIES IF NOT INSTALLED, AND LOAD THEM
packages <- c(
'grid',
'ggplot2',
'gridExtra',
'ggthemes',
'tidytext',
'dplyr',
'tibble',
'ggbeeswarm',
'readr'
)
lapply(packages, \(x) if (!require(x, character.only = TRUE)) { install.packages(x)
	library(x, character.only = TRUE) })

## globasl themes and color scales
theme_set(theme_economist(
       base_size = 10,
       base_family = 'sans',
       horizontal = TRUE,
       dkpanel = TRUE))
options(
	ggplot2.continuous.colour = scale_colour_stata,
	ggplot2.continuous.fill = scale_fill_stata,
	ggplot2.discrete.colour = scale_colour_stata,
	ggplot2.discrete.fill = scale_fill_stata)

compute_lm <- function(dataset, resp_name, pred_name, significance_threshold = 0.05, r_min_threshold = 0.09)
{
	list_entry <- paste0(resp_name, '_~_', pred_name)
	## ADD PAIR TO THE KEEPING TRACK LODGER
	## CHECK THE INVERTED PAIR HAS NOT BEEN PROCESSED
	if(any(paste0(pred_name, resp_name) %in% pairs_already_processed) || any(paste0(resp_name, pred_name) %in% pairs_already_processed)) return(NULL)
	## ADD TO ALREADY PROCESSED KEEPING TRACK LIST
	## NOTE THAT IF THE REGRESSION-RESPONSE VARIABLE IS FACTOR, THE PAIR SHOUL NOT BE ADDED, SINCE THE REVERSED ANALYSIS STILL MUST BE DONE
	if(!is.numeric(dataset[[resp_name]])) pairs_already_processed[[list_entry]] <<- paste0(resp_name, pred_name)
	## CHECK THAT RESPONSE AND PREDICTOR ARE NOT THE SAME
	if(resp_name == pred_name) return(NULL)
	## RESPONSE VARIABLE IS NUMERIC AND NOT NA
	if(!is.numeric(dataset[[resp_name]])) return(NULL)
	## CHECKING THAT THERE IS ENOUGH DIFFERENT LEVELS OR VARIANCE
	if(length(unique(dataset[[pred_name]])) < 2) return(NULL)
	## GETTING P-VALUE AND COEFFIECIETS
	formula <- paste(resp_name, '~', pred_name)
	lmsummary <- dataset |> lm(formula, data = _, na.action = 'na.exclude') |> summary()
	lmadjr <- lmsummary[['adj.r.squared']] |> signif(digits = 2)
	lmr <- lmsummary[['r.squared']] |> signif(digits = 2)
	lmcoeffs <- lmsummary |> coef()
	vector_pvalues <- lmcoeffs[-1, 'Pr(>|t|)']
	min_pvalue <- min(vector_pvalues) |> signif(digits = 1)
	lmestimates <- lmcoeffs[, 'Estimate']
	## CHEKC IF FITTED WITH PREDICTORS THAT DO NOT VARY, BY MAKING SURE THE LMESTIMATES HAVE MORE THAN JUST THE INTERCEPT
	if(length(lmestimates) < 2) return(NULL)
	## RENAME THE NON-INTERCEPT ESTIMATE AS SLOPE
	names(lmestimates)[2] <- 'slope'
	addee_vector <- c('response' = resp_name, 'predictor' = pred_name, 'min_pvalue' = min_pvalue, 'rsqr' = lmr, 'adjr' = lmadjr, lmestimates, vector_pvalues)
	## CHECH IF IS SIGNIFICAT, THEN ADD TO LISTN AND RETURN PVAL
	if(!is.na(min_pvalue) && !is.na(lmadjr) && min_pvalue < significance_threshold && lmadjr > r_min_threshold)
	{
		significant_pvalues_r2_list[[paste(resp_name, pred_name)]] <<- c('pv' = min_pvalue, 'R2' = lmadjr)
		print(paste(resp_name, pred_name))
		cat('pv= ', min_pvalue, '\n', 'R2= ', lmadjr, '\n', sep='')
		## ADD ENTRY TO THE SIGNIFICAT RESULTS LIST
		significant_analyses_list[[list_entry]] <<- addee_vector
		min_pvalue
	}
}


mapped_analyze_multiple_nested_significat_lm <- function(dataset, respcols = names(dataset), predcols = names(dataset), significance_threshold = 0.05, r_min_threshold = 0.09, decreasing = TRUE)
{
	assign('significant_analyses_list', NULL, pos = 1)
	assign('pairs_already_processed', NULL, pos = 1)
	assign('significant_pvalues_r2_list', NULL, pos = 1)
	results_matrix_lm <- sapply(respcols, \(each_resp) sapply(predcols, \(each_pred) compute_lm(dataset, each_resp, each_pred, significance_threshold = significance_threshold, r_min_threshold = r_min_threshold)))
	## ORDER DECREASIN SIGNIFICANT ANALYSES LIST
	significant_analyses_list[] <<- significant_analyses_list[names(significant_analyses_list)[sapply(names(significant_analyses_list), \(each_signame) significant_analyses_list[[each_signame]][['adjr']]) |> as.numeric() |> order(decreasing = decreasing)] ]
	print('NUMBER OF SIGNIFICANT ANALYSES:')
	print(length(significant_analyses_list))
	list('pr2' = significant_pvalues_r2_list, 'summpar' = significant_analyses_list, 'pvalues' = results_matrix_lm)
}



add_significant_jitter_plots <- function(plotee_dataset, significant_analyses_list = significant_analyses_list, opacity = 0.5)
{
	## CHECK IT IS NOT EMPTY LIST
	if(length(significant_analyses_list) < 1) return(NULL)
	sapply(significant_analyses_list, function(each_resp_pred)
       {
		ggplot(plotee_dataset, aes(!!as.symbol(each_resp_pred['predictor']), !!as.symbol(each_resp_pred['response']))) +
			geom_jitter(color = '#333366', alpha = opacity) +
			 #geom_smooth(method = "lm")
			geom_abline(
				    slope = as.numeric(each_resp_pred['slope']),
				    intercept = as.numeric(each_resp_pred['(Intercept)']),
				    color = '#663333'
			)
       }
	, USE.NAMES = TRUE, simplify = FALSE)
}

write_graphics_using_device_png_or_pdf <- function(writee_pgrid, save_graph_to = 'z.png')
{
	## SAVE TO PDF
	if(grepl('\\.pdf$', save_graph_to)) pdf(save_graph_to, width = 7, height = 7, bg = 'white')
	## SAVE TO PNG
	if(grepl('\\.png$', save_graph_to)) png(save_graph_to, width = 480, height = 480, bg = 'white')
	## SAVE TO JPEG
	if(grepl('\\.jpeg$', save_graph_to)) jpeg(save_graph_to, width = 480, height = 480, bg = 'white')
	grid.draw(writee_pgrid)
	dev.off()
}

						   
compute_and_save_grid_plots <- function(plot_list = plot_list, save_graph_to = 'z.png')
{
	## CHECK IT IS NOT EMPTY LIST
	nump <- length(plot_list)
	if(nump < 1) return(NULL)
	#pgrid <- do.call('grid.arrange', c(plot_list, nrow = round(sqrt(nump))))
	#pgrid <- marrangeGrob(plot_list, ncol = 4, nrow = ifelse(nump > 3, 4, 1 + floor(nump / 4)))
	if(nump < 17) pgrid <- do.call('grid.arrange', c(plot_list, nrow = round(sqrt(nump))))
	else pgrid <- marrangeGrob(plot_list, ncol = 4, nrow = 4, layout_matrix = matrix(1:16, 4, 4, byrow = TRUE) )
	pgrid
}


add_significant_conditional_jitter_or_dotplot <- function(plotee_dataset, significant_analyses_list = significant_analyses_list, opacity = 0.5, scatter_cats = FALSE)
{
	## CHECK IT IS NOT EMPTY LIST
	if(length(significant_analyses_list) < 1) return(NULL)
	sapply(significant_analyses_list, function(each_resp_pred)
	{
		predictor_name_or_category <- each_resp_pred[['predictor']]
		response_name <- each_resp_pred[['response']]
		predictor_column <- plotee_dataset[[predictor_name_or_category]]
		slope <- as.numeric(each_resp_pred['slope'])
		intercept <- as.numeric(each_resp_pred['(Intercept)'])
		if(scatter_cats || is.numeric(predictor_column))
		{
			#print('____NUMERIC_NOT_FACTOR_OR_CHARACTER____')
			#print(predictor_name_or_category)
		## OLD JITTER GEOM FUNCTION
		p <- ggplot(plotee_dataset, aes(!!as.symbol(predictor_name_or_category), !!as.symbol(response_name))) +
			geom_jitter(color = '#333366', alpha = opacity) +
			labs(x = predictor_name_or_category, y = response_name, caption = paste0('p=', each_resp_pred[['min_pvalue']], ', R2=', each_resp_pred[['adjr']]))
			## ADD ERROR BARS OR REGRESSION LINE
		if(!is.numeric(predictor_column)) p <- p + stat_summary(fun.data = 'mean_se', geom = 'errorbar', fun.args = list(mult = 1.96), width = 0.4, color = '#663333') +
			stat_summary(fun = mean, geom = "point", shape = 5, size = 1.0, color = '#663333')
		else p <- p + geom_abline(slope = slope, intercept = intercept, color = '#663333')
		return(p)
		}
		else
		{
		#print('____NOT_NUM_DOTPLOT_FOR_CHAR/FACT___')
		#print(predictor_name_or_category)
		plotee_dataset %>%
		mutate(
		differencee_response = !!as.symbol(response_name),
		cat_noordered = !!as.symbol(predictor_name_or_category),
		cat_reordered = reorder_within(cat_noordered, differencee_response, cat_noordered)) %>%
#reorder_within(x, by, within, fun = mean, sep = "___", ...)
#scale_x_reordered(..., labels = reorder_func, sep = deprecated())
		ggplot(aes(cat_reordered, differencee_response)) +
			## NEW DOTPLOT FUNCTION
			geom_dotplot(aes(color = cat_reordered, fill = cat_reordered), binaxis = 'y', binpositions = 'all', stackdir = 'center', dotsize = 0.6, stackratio = 1.00, position = position_jitter(width = 0.00, height = 0.04), alpha = opacity, show.legend = FALSE) +
			##  USE GGBEESWARM QUASIRANDOM INSTEAD OF DOTPLT
			#ggbeeswarm::geom_quasirandom(aes(color = cat_reordered, fill = cat_reordered), alpha = opacity, show.legend = FALSE) +
			## USE COLORED BOXPOLTS
			#geom_boxplot(fill = NA, color = 'grey60', alpha = 0.2, show.legend = FALSE) +
			geom_boxplot(aes(color = cat_reordered), fill = NA, alpha = opacity, show.legend = FALSE) +
			## COPYED ROM CATEGORICAL GITHUB
			stat_summary(fun.data = 'mean_se', geom = 'errorbar', fun.args = list(mult = 1.96), width = 0.4) +
			#stat_summary(fun.data = 'mean_se', geom = 'pointrange', alpha = 0.4, color = '#663333') +
			stat_summary(fun = mean, geom = "point", shape = 5, size = 1.0) +
			## REORDER_WITIN SECOND COMPONENT
			scale_x_reordered() +
			labs(x = predictor_name_or_category, y = response_name, caption = paste0('p=', each_resp_pred[['min_pvalue']], ', R2=', each_resp_pred[['adjr']]))
		}
       }, USE.NAMES = TRUE, simplify = FALSE)
}


## MAKE AND SAVE GRID OF GRAPHICS
grid_from_significants_list_conditional_jitter_dotplot <- function(dataset, significant_analyses_list, opacity = 0.05, save_graph_to = 'z.png', scatter_cats = FALSE)
{
	if(is.null(significant_analyses_list)) return(list('grid' = NULL, 'plots' = NULL))
	## MAKE AND SAVE GRID OF GRAPHICS
	plot_list <-add_significant_conditional_jitter_or_dotplot(dataset, significant_analyses_list, opacity = opacity, scatter_cats = scatter_cats)
	pgrid <- compute_and_save_grid_plots(plot_list, save_graph_to = save_graph_to)
	print('NUMBER OF GRAPHICS IN GRID:')
	print(length(plot_list))
	list('grid' = pgrid, 'plots' = plot_list)
}


## FUNCTION TO BE STORED AND WHEN CALLED SHOWS THE SUMMARY PARAMETERS
show_pars <- function(showee_significant_analyses_summary_list, decreasing = FALSE)
{
	summary_parameters_list <- showee_significant_analyses_summary_list[['pr2']]
	## ORDERING LIST BY R2 VALUES
	rordered <- summary_parameters_list |> sapply(\(x) x[2]) |> order(decreasing = decreasing)
	invisible(lapply(names(summary_parameters_list)[rordered], \(each_name)
	 {
	 parameter_pair <- summary_parameters_list[[each_name]]
	 cat(each_name, '\n', 'pv= ', parameter_pair[1], '\n', 'R2= ', parameter_pair[2], '\n', sep = '')
	 }))
	cat('TOTAL NUMBER OF SIGNIFICANT ANALISES: ', length(summary_parameters_list), '\n', sep = '')
}


regression_significant_main <- function(dataset, respcols = names(dataset), predcols = names(dataset), significance_threshold = 0.05, r_min_threshold = 0.09, make_graphics = FALSE, opacity = 0.5, save_graph_to = 'z.png', scatter_cats = FALSE, decreasing = TRUE)
{
	significants_pvalues_list <- mapped_analyze_multiple_nested_significat_lm(dataset, respcols, predcols, significance_threshold = significance_threshold, r_min_threshold = r_min_threshold, decreasing = decreasing)
	## MAKE AND SAVE GRID OF GRAPHICS
	if(make_graphics) grid_plots_list <- grid_from_significants_list_conditional_jitter_dotplot(dataset, significant_analyses_list, opacity = opacity, save_graph_to = save_graph_to, scatter_cats = scatter_cats)
	else grid_plots_list <- list('grid' = NULL, 'plots' = NULL)
	## WRITE GRPAPHIC TO DISK
	if(length(grid_plots_list[['grid']]) > 0 && any(grepl('\\.(pdf|png|jpeg)$', save_graph_to))) write_graphics_using_device_png_or_pdf(grid_plots_list[['grid']], save_graph_to = save_graph_to) #ggsave(grid_plots_list[['grid']], file = save_graph_to)
	append(significants_pvalues_list, grid_plots_list)
}
