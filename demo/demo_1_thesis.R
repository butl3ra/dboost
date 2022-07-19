# --- set seed
set.seed(1)
load_dboost_modules()

# --- cone program:
n_z = 2
n_obs = 1000
eps = 10
A = matrix(1,1,n_z)
b = 1
G = -diag(n_z)
h = matrix(0,n_z)#rep(0,n_z)

# --- lp cone:
control = scs::scs_control(acceleration_lookback = 7,eps_rel = 1e-02, eps_abs = 1e-02)
cone_lp = lp_cone_prep(A = A,
                       b = b,
                       G = G,
                       h = h,
                       cone = list(z = 0, l = 0, q = NULL))

# --- generate data:
x = runif(n_obs,-2,2)
x = x[order(x)]
x = matrix(x)

# --- synthetic costs:
P = eps*diag(n_z)
tau = 0.5
cost1 = x^3 + 10 + tau*rnorm(n_obs)
cost2 = x - 8*x^2 + 20 + tau*rnorm(n_obs)
cost = cbind(cost1,cost2)

plot(x,cost1,type='p',lwd=3,pch=16)
points(x,cost2,col = 'darkred',lwd=4,pch=16)

# --- create oracle:
opt_oracle_lp = optim_scs(A = cone_lp$A,
                          b = cone_lp$b,
                          P = P,
                          cone = cone_lp$cone,
                          control = control)

# --- z_star true:
z_star = opt_oracle_lp(cost,P = P)
total_cost_star = sum(compute_qcost(z_star,cost,P))

# --- mse boosting fit:
b_mse = boost_fit(x = x,
                  y = cost,
                  demean = FALSE,
                  model_method = 'dtree_fit',
                  model_args = list(max_depth = 0,
                                    min_obs = 0.01,
                                    step_size = 0.01,
                                    objective_method = 'objective_dtree_pve',
                                    maximize = NULL,
                                    y_hat_fn = NULL),
                  objective_method = 'objective_mse',
                  grad_method = 'grad_mse',
                  do_grad_project = TRUE,
                  maximize = NULL,
                  verbose = TRUE,
                  max_iter = 100,
                  weight_tol = 0.01,
                  objective_tol = 10^-4)

# --- full predictions:
cost_hat_mse = predict(b_mse,x=x)
z_hat_mse = opt_oracle_lp(cost_hat_mse)
total_cost_mse = sum(compute_qcost(z_hat_mse,cost,P))


# --- decision aware: QSPO
b_spo = boost_fit(x = x,
                  y = cost,
                  demean = FALSE,
                  model_method = 'dtree_fit',
                  model_args = list(max_depth = 0,
                                    min_obs = 0.01,
                                    step_size = 0.01,
                                    objective_method = 'objective_dtree_qspo',
                                    maximize = NULL,
                                    y_hat_fn = NULL),
                  objective_method = 'objective_qspo',
                  grad_method = 'grad_qspo',
                  do_grad_project = TRUE,
                  maximize = NULL,
                  verbose = TRUE,
                  max_iter = 100,
                  weight_tol = 0.01,
                  objective_tol=10^-4,
                  opt_oracle = opt_oracle_lp)

cost_hat_spo = predict(b_spo,x=x)
z_hat_spo = opt_oracle_lp(cost_hat_spo)
total_cost_spo = sum(compute_qcost(z_hat_spo,cost,P))

(total_cost_mse-total_cost_star)/total_cost_star
(total_cost_spo-total_cost_star)/total_cost_star

# --- plot and prediction:
lwd = 3
pch = 16
lty=2
ylab = 'Return'
xlab = 'x'
cex.lab = 1.25
cex.axis = 1.25
ylim = c(-10/100,40/100)
yticks_val <- scales::pretty_breaks(n=6)(seq(-0.2,0.4,0.01))

dir_out = '~/Dropbox (Personal)/'
pdf(paste0(dir_out,'dboost_2.pdf'))

plot(x,cost1/100,lwd=lwd,ylim=ylim,pch=pch,yaxt="n",col = 'navy',ylab = ylab,cex.lab = cex.lab,cex.axis = cex.axis)
points(x,cost2/100,col = 'darkred',lwd = lwd,pch = pch)
axis(2, at=yticks_val, lab=scales::percent(yticks_val),cex.axis=cex.axis)
lines(x,cost_hat_mse[,1]/100,col='lightblue',lwd=2*lwd)
lines(x,cost_hat_mse[,2]/100,col='pink',lwd=2*lwd)

legend(-2, 0.4,
       legend=c("Return 1 (True) ", "Return 2 (True)",'Return 1 (MSE)','Return 2 (MSE)'),
       col=c("navy", "darkred",'lightblue','pink'),
       lwd = c(2,2,4,4,4,4),
       pch = c(16,16,-1,-1,-1,-1),
       lty=c(0,0,1,1),
       cex=1.25,
       box.lty = 0)
dev.off()


pdf(paste0(dir_out,'dboost_2_spo.pdf'))

plot(x,cost1/100,lwd=lwd,ylim=ylim,pch=pch,yaxt="n",col = 'navy',ylab = ylab,cex.lab = cex.lab,cex.axis = cex.axis)
points(x,cost2/100,col = 'darkred',lwd = lwd,pch = pch)
axis(2, at=yticks_val, lab=scales::percent(yticks_val),cex.axis=cex.axis)
lines(x,cost_hat_spo[,1]/100,col='lightblue',lwd=2*lwd)
lines(x,cost_hat_spo[,2]/100,col='pink',lwd=2*lwd)

legend(-2, 0.4,
       legend=c("Return 1 (True) ", "Return 2 (True)",'Return 1 (dboost)','Return 2 (dboost)'),
       col=c("navy", "darkred",'lightblue','pink'),
       lwd = c(2,2,4,4,4,4),
       pch = c(16,16,-1,-1,-1,-1),
       lty=c(0,0,1,1),
       cex=1.25,
       box.lty = 0)
dev.off()

plot(x,cost1,type='p',lwd=3,pch=16,col='navy')
points(x,cost2,col = 'darkred',lwd=4,pch=16)
lines(x,cost_hat_mse[,1],col='grey',lwd=5)
lines(x,cost_hat_mse[,2],col='pink',lwd=5)
lines(x,cost_hat_spo[,1],col='navy',lwd=5)
lines(x,cost_hat_spo[,2],col='coral',lwd=5)



