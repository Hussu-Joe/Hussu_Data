#!/usr/bin/env python
# coding: utf-8

# In[1]:


import tensorflow.keras as keras
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import RNN,Dense,SimpleRNN, Activation,LSTM,Dropout,Bidirectional,GRU
from tensorflow.keras.callbacks import EarlyStopping
from tensorflow.keras.optimizers import Adam, Adamax, Adagrad, SGD, RMSprop, Adadelta
from tensorflow.keras.preprocessing.sequence import TimeseriesGenerator
from tensorflow.compat.v1.train import MomentumOptimizer
from scipy.stats import boxcox
from scipy.special import inv_boxcox, boxcox1p
import numpy as np
import pandas as pd
import tensorflow as tf
from tensorflow import keras
import keras.backend.tensorflow_backend
from collections import Counter
from sklearn.metrics import mean_squared_error, mean_absolute_error
from sklearn.preprocessing import MinMaxScaler, OneHotEncoder
import pymysql
from keras.utils import plot_model
import datetime
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor


# In[2]:


config = tf.compat.v1.ConfigProto()
config.gpu_options.allow_growth = True
sess = tf.compat.v1.Session(config = config)
keras.backend.tensorflow_backend.set_session(sess)


# In[3]:


pv = pd.read_csv("./nemo1_preproc_20200303.csv",index_col=0)
pv.PS = pv.PS.interpolate()


# In[4]:


dark = pd.read_csv("./dark_test.csv",index_col=0).reset_index(drop=True)
dark.DC10_TCA = dark.DC10_TCA*10
dark.HM = dark.HM*100
dark.HMMAX = dark.HMMAX*100
dark.HMMIN = dark.HMMIN*100


# In[5]:


pv2=pv.copy()


# In[450]:


# st["TM4"]=st.TA +(st.rad_dc/1000)*(0.0712*(st.WS2)-2.411*st.WS+32.96)


# In[6]:


pv2["WS2"]=(pv2.WS)**2
pv2["HMxDC"] = pv2.HM*pv2.DC10_TCA
pv2["TAxHM"] = pv2.TA*pv2.HM
pv2["HMxWS"]=pv2.HM*pv2.WS
pv2["TM4"] = pv2.TA +(pv2.rad/1000)*(0.0712*(pv2.WS2)-2.411*pv2.WS+32.96)


# In[7]:


dark["WS2"]=(dark.WS)**2

dark["HMxDC"] = dark.HM*dark.DC10_TCA
dark["TAxHM"] = dark.TA*dark.HM
dark["HMxWS"]=dark.HM*dark.WS
dark["TM4"] = dark.TA +(dark.rad/1000)*(0.0712*(dark.WS2)-2.411*dark.WS+32.96)


# In[8]:


pv3 = pv2[:-24*30]


# In[9]:


model1000 = ["TA","HM","WS","WS2","DC10_TCA","rad","HMxDC","HMxWS","TM4","TAMAX","HMMIN","TAxHM","PS","SUMRN","SUMDSNW"]


# In[13]:


pv4 = pv2[pv2.date=="2020-01-31"].reset_index(drop=True)


# In[16]:


pv5 = pv4.loc[:,model1000]


# In[19]:


pv5


# In[229]:


# LSTM 용
Y=pv3.watts.values
X = pv3.loc[:,model1000]
 
#
aaaa = pv4.copy().reset_index(drop=True)
TA = aaaa.TA.values.astype('float32')
X_ = aaaa.loc[:,model1000]


# In[44]:


test_watts=pv2[-24*30:].watts.values
test_date = pv2[-24*29:].date.values


# In[33]:


# LSTM 용
watts=pv3.watts.values
train_y = watts.reshape(len(watts),1)
train_x = np.array(pv3.loc[:,model1000],dtype=np.float32)
feature_num = train_x.shape[1]
data_gen = TimeseriesGenerator(train_x, train_y,length=24, sampling_rate=1,stride=1,batch_size=len(train_x))
X, Y =data_gen[0]
#
aaaa=dark.copy().reset_index(drop=True)
aaaa2 = aaaa.loc[:,model1000]
aaaa3 = pv5.append(aaaa2)
TA = aaaa3.TA.values.astype('float32')
test_y =TA.reshape(len(TA),1)
test_x = np.array(aaaa3,dtype=np.float32)
data_gen = TimeseriesGenerator(test_x, test_y,length=24, sampling_rate=1,stride=1,batch_size=len(test_x))
X_, Y_ =data_gen[0]

n_hidden=500


# In[34]:


# LSTM 
model = Sequential() 
model.add(LSTM(n_hidden,kernel_initializer=tf.initializers.glorot_normal,return_sequences=True,input_shape=(24, feature_num)))
model.add(LSTM(n_hidden,return_sequences=True))
model.add(LSTM(n_hidden))
model.add(Dense(1, kernel_initializer=tf.initializers.glorot_normal))
model.add(Activation('elu'))

## 옵티마이져
#optimizer = Adam(lr=0.0001, beta_1 = 0.9, beta_2 = 0.999)    # ADAM 옵티마이져, loss Mse
#optimizer = RMSprop(learning_rate=0.001, rho= 0.9, momentum = 0.0, epsilon=0.0000001)    # ADAM 옵티마이져, loss Mse
#optimizer = SGD(learning_rate=0.0001,momentum=0.9,nesterov=True) #SGD는 별로 도움이 안됨
optimizer = Adamax(lr=0.001, beta_1 = 0.9, beta_2 = 0.999)
#optimizer = Adagrad(lr=0.001, epsilon=1e-6) X
#optimizer = Adadelta(lr=1.0, rho=0.95, epsilon=None, decay=0.0)
#@optimizer = MomentumOptimizer(learning_rate=0.001, momentum=0.9) 못씀
model.compile(loss = 'mean_absolute_error',
             optimizer=optimizer)


# In[181]:


batch_size = 12
epochs = 100
model.fit(X, Y,batch_size=batch_size,epochs=epochs)


# In[45]:


batch_size = 24
mae=[1]
epochs = 1
for i in range(0,100):
    print(i+1)
    model.fit(X, Y,
         batch_size=batch_size,
         epochs=epochs)
    t=pd.DataFrame({"date":test_date,"pred":model.predict(X_).reshape(len(X_,)),"hour":np.tile(range(0,24),(int(len(X_)/24),)),
                    "num":range(0,24*(int(len(X_)/24))),"real":test_watts[24:]})
    ax=t.plot(x="num",y="pred",legend=False)
    t.plot(x="num",y="real",color="r",ax=ax,legend=False)
    ax.yaxis.grid()
    ax.xaxis.grid()
    ax.figure.legend()
    plt.title("PV prediction")
    plt.show()
    plt.close()
    method=["nn"]
    eval_df = pd.DataFrame({})
    t["AE"] = abs(t.real-t.pred)/498
    nMAE = [np.mean(t.AE)]
    eval_df["method"]=method
    eval_df["nMAE"]=nMAE
    print(eval_df)
    if(eval_df.nMAE.values[0]<np.min(mae)):
        model.save_weights("./dark_nemo1)20200303_lstm.h5")
    mae += [eval_df.nMAE.values[0]]


# In[46]:


model.load_weights("./dark_nemo1)20200303_lstm.h5")


# In[47]:


t=pd.DataFrame({"date":test_date,"pred":model.predict(X_).reshape(len(X_,)),"hour":np.tile(range(0,24),(int(len(X_)/24),)),
                    "num":range(0,24*(int(len(X_)/24))),"real":test_watts[24:]})


# In[50]:


t["nmae"]=abs(t.real-t.pred)/498


# In[51]:


t["rate"]= t.real/498


# In[52]:


tt=t[t.rate>=0.1]


# In[53]:


tt["wl"]=np.repeat(np.nan,len(tt))


# In[54]:


tt.wl[tt.nmae<=0.08]=1
tt.wl[tt.wl.isnull()]=0


# In[55]:


ttt = tt[tt.wl==1]


# In[56]:


ttt["incen"]=ttt.real*3


# In[61]:


ttt


# In[58]:


np.mean(tt.nmae)


# In[60]:


sum(ttt.incen)


# In[14]:


model2 =  ["ICSR_FIT","SS_FIT","ICSRFITxSSFIT",
          "SSFIT_2","ICSRFIT_2",'M0', 'M1', 'M2', 'M3',
       'M4', 'M5', 'M6', 'M7', 'M8', 'M9', 'M10', 'M11',
          "logTA", "logHM","logTAxlogHM","logTA_2","logHM_2","TCA0","TCA1","TCA2","Tm0sq"]


# In[317]:


model2 =  ["Tm0sq","log_Tm0sq_2"]


# In[139]:


model2 =  ["logTA", "logHM","logTAxlogHM",'ICSR_FIT',"ICSRFIT_lag1","ICSRFIT_lag2","TCA0","TCA1","TCA2",
          'logHMxlogTA_2', 'logHMxTCA0','logHMxTCA1','logHMxTCA2',
          "TCA0_lag1","TCA1_lag1","TCA2_lag1","TCA0_lag2","TCA1_lag2","TCA2_lag2","ICSRFITxSSFIT"
           ,'M0', 'M1', 'M2', 'M3',
       'M4', 'M5', 'M6', 'M7', 'M8', 'M9', 'M10', 'M11',"log_rad_M","log_watts_pred","DC10_sqrt","Tm0sq","delta"]


# In[87]:


model2 =  ["log_rad_M","log_watts_pred","DC10_sqrt","Tm0sq","delta"]


# In[15]:


bandi1 = pd.read_csv("./bandi_used_feature.csv")


# In[16]:


bandi1 = bandi1[:-72]


# In[17]:


bandi1_test = pd.read_csv("./bandi_used_feature_test.csv")


# In[18]:


bandi1_test = bandi1_test[:-72]


# In[ ]:





# In[41]:





# In[19]:


df2["rad_M"]=bandi1_test.radiation_M
df2["watts_pred0_sq"]=bandi1_test.watts_pred0_sq
df2["DC10_sqrt"]=bandi1_test.DC10_sqrt_sm3
df2["Tm0sq"]=bandi1_test.Tm0sq
df2["delta"]=bandi1_test.delta
df2["log_rad_M"]=np.log(bandi1_test.radiation_M+1)
df2["log_watts_pred"]=np.log(bandi1_test.watts_pred0_sq+1)
df2["log_Tm0sq"]=np.log(df2.Tm0sq+273)
df2["log_Tm0sqxTCA0"]=df2.log_Tm0sq*df2.TCA0
df2["log_Tm0sqxTCA1"]=df2.log_Tm0sq*df2.TCA1
df2["log_Tm0sqxTCA2"]=df2.log_Tm0sq*df2.TCA2
df2["Tm0sq_2"]=(df2.Tm0sq)**2
df2["log_Tm0sq_2"]=np.log(df2.Tm0sq_2+1)


# In[ ]:





# In[ ]:





# In[20]:


df["rad_M"]=bandi1.radiation_M
df["watts_pred0_sq"]=bandi1.watts_pred0_sq
df["DC10_sqrt"]=bandi1.DC10_sqrt_sm3
df["Tm0sq"]=bandi1.Tm0sq
df["delta"]=bandi1.delta
df["log_rad_M"]=np.log(bandi1.radiation_M+1)
df["log_watts_pred"]=np.log(bandi1.watts_pred0_sq+1)
df["log_Tm0sq"]=np.log(df.Tm0sq+273)
df["log_Tm0sqxTCA0"]=df.log_Tm0sq*df.TCA0
df["log_Tm0sqxTCA1"]=df.log_Tm0sq*df.TCA1
df["log_Tm0sqxTCA2"]=df.log_Tm0sq*df.TCA2
df["Tm0sq_2"]=(df.Tm0sq)**2
df["log_Tm0sq_2"]=np.log(df.Tm0sq_2+1)


# In[170]:


df.DC10_TCA


# In[21]:


dong_test=df2[df2["date"].isin(["2019-02-23","2019-02-24","2019-02-25","2019-02-26","2019-02-27","2019-02-28",
                     "2019-03-27","2019-03-28","2019-03-29","2019-03-30","2019-03-31",
                     "2019-04-26","2019-04-27","2019-04-28","2019-04-29","2019-04-30",
                     "2019-05-27","2019-05-28","2019-05-29","2019-05-30","2019-05-31",
                     "2019-06-26","2019-06-27","2019-06-28","2019-06-29","2019-06-30",
                     "2019-07-27","2019-07-28","2019-07-29","2019-07-30","2019-07-31",
                     "2019-08-27","2019-08-28","2019-08-29","2019-08-30","2019-08-31",
                     "2019-09-26","2019-09-27","2019-09-28","2019-09-29","2019-09-30",
                     "2019-10-27","2019-10-28","2019-10-29","2019-10-30","2019-10-31",
                     "2019-11-26","2019-11-27","2019-11-28","2019-11-29","2019-11-30",
                     "2019-12-27","2019-12-28","2019-12-29","2019-12-30","2019-12-31",
                     "2020-01-27","2020-01-28","2020-01-29","2020-01-30","2020-01-31"])]


# In[22]:


jong_test= df.iloc[dong_test.index,:]


# In[187]:


df_2=df2.drop(index=(dong_test.index))


# In[23]:


df_1=df.drop(index=(dong_test.index))


# In[47]:


aaaaaaaaa=df_1.append(dong_test).loc[:,model2]


# In[293]:


aaaaaaaaa=aaaaaaaaa.sort_index()


# In[294]:


aaaaaaaaa.to_csv("./jonah_feature3.csv",sep=",")


# In[54]:


# watts=df_1.watts.values.astype('float32')
# dataset_x = df_1.loc[:,model2]
# dataset_y = watts


# In[92]:


# aaaa=dong_test.copy().reset_index(drop=True)
# aaaa2=aaaa.loc[:,model2]


# In[93]:


# rf_reg=rf.fit(dataset_x,dataset_y)


# In[94]:


# t=pd.DataFrame({"date":aaaa.date,"pred_watts":rf_reg.predict(aaaa2),"hour":np.tile(range(0,24),(int(len(aaaa2)/24),)),
#                     "num":range(0,24*(int(len(aaaa2)/24))),"real_watts":aaaa.watts})


# # LSTM

# In[126]:


df_1.isnull().any()[:24]


# In[25]:


# LSTM 용
watts=df_1.watts.values.astype('float32')
dataset_x = np.array(df_1.loc[:,model2],dtype=np.float32)
dataset_y = watts.reshape(len(watts),1)
feature_num = dataset_x.shape[1]
data_gen = TimeseriesGenerator(dataset_x, dataset_y,length=24, sampling_rate=1,stride=1,batch_size=len(dataset_x))
X, Y =data_gen[0]
train_x, test_x, train_y, test_y = X[0:284*24+46*24,:],X[284*24+46*24:,:],Y[0:284*24+46*24,:],Y[284*24+46*24:,:]
tr_x, val_x, tr_y, val_y = train_x[:284*24,:],train_x[284*24:,:], train_y[:284*24,:],train_y[284*24:,:]
n_hidden = 500

aaaa=dong_test.copy().reset_index(drop=True)
TA = aaaa.TA.values.astype('float32')
data_x = np.array(aaaa.loc[:,model2],dtype=np.float32)
data_y = TA.reshape(len(TA),1)
#feature_num = data_x.shape[1]
data_gen = TimeseriesGenerator(data_x, data_y,length=24, sampling_rate=1,stride=1,batch_size=len(data_x))
X_, Y_ =data_gen[0]


# In[26]:


# LSTM 
model = Sequential() 
model.add(LSTM(n_hidden,kernel_initializer=tf.initializers.glorot_normal,return_sequences=True,input_shape=(24, feature_num)))
model.add(LSTM(n_hidden,return_sequences=True))
model.add(LSTM(n_hidden))
model.add(Dense(1, kernel_initializer=tf.initializers.glorot_normal))
model.add(Activation('elu'))

## 옵티마이져
#optimizer = Adam(lr=0.0001, beta_1 = 0.9, beta_2 = 0.999)    # ADAM 옵티마이져, loss Mse
#optimizer = RMSprop(learning_rate=0.001, rho= 0.9, momentum = 0.0, epsilon=0.0000001)    # ADAM 옵티마이져, loss Mse
#optimizer = SGD(learning_rate=0.0001,momentum=0.9,nesterov=True) #SGD는 별로 도움이 안됨
optimizer = Adamax(lr=0.001, beta_1 = 0.9, beta_2 = 0.999)
#optimizer = Adagrad(lr=0.001, epsilon=1e-6) X
#optimizer = Adadelta(lr=1.0, rho=0.95, epsilon=None, decay=0.0)
#@optimizer = MomentumOptimizer(learning_rate=0.001, momentum=0.9) 못씀
model.compile(loss = 'mean_squared_error',
             optimizer=optimizer)


# In[143]:


# 얼리스타핑
# epochs = 200
# batch_size = 24
# early_stopping = EarlyStopping(monitor='val_loss',patience=10, verbose=1)
# model.fit(X, Y,
#          batch_size=batch_size,
#          epochs=epochs)


# In[28]:


batch_size = 24
mae=[]
epochs = 1
for i in range(0,100):
    print(i+1)

    model.fit(X, Y,
         batch_size=batch_size,
         epochs=epochs)
    t=pd.DataFrame({"date":aaaa.date[24:],"pred":model.predict(X_).reshape(len(X_,)),"hour":np.tile(range(0,24),(int(len(X_)/24),)),
                    "num":range(0,24*(int(len(X_)/24))),"real":aaaa.watts[24:]})
# t["APE"] = ((abs(t.real-t.pred))/abs(t.real))*100
    print(len(aaaa.watts.values[24:]))
    ax=t[480:720].plot(x="num",y="pred",legend=False)
    t[480:720].plot(x="num",y="real",color="r",ax=ax,legend=False)
    ax.yaxis.grid()
    ax.xaxis.grid()
    ax.figure.legend()
    plt.title("PV prediction")
    plt.show()
    plt.close()
    method=["nn"]
    eval_df = pd.DataFrame({})
    t["AE"] = abs(t.real-t.pred)
    t["SE"] = (t.real-t.pred)**2
    MAE = [sum(t.AE)/(len(t.AE))]
    MSE = [sum(t.SE)/len(t.SE)]
    eval_df["method"]=method
    eval_df["MAE"]=MAE
    eval_df["MSE"]=MSE
    print(eval_df)
    mae += [eval_df.MAE.values[0]]
#     if ((eval_df.MAE.values[0]<=np.min(mae))):
#         model.save_weights("pv_20200210_bandi_m2_"+str(i)+".h5")
#plt.savefig("./pv2_nemo1.png",dpi=400)
    


# In[152]:


model.load_weights("./pv_20200210_bandi_m1_21.h5")


# In[124]:


#X_ = test_x.copy()
# watts2 = [test_x[0][23][1]]
# watts3 = [test_x[0][22][1],test_x[0][23][1]]
# time2 = []
pred =[]
# sr2 = []


for i in range(0,(len(X_)-1)):
    pre =model.predict(X_[i].reshape(1,24,feature_num)).reshape(1)[0]
    X_[i+1].reshape(1,24,feature_num)[0][23][0]=pre
#     X_[i+1].reshape(1,24,feature_num)[0][23][1]=X_[i].reshape(1,24,feature_num)[0][23][0]
#     X_[i+1].reshape(1,24,feature_num)[0][23][2]=X_[i].reshape(1,24,feature_num)[0][23][1]
#     X_[i+1].reshape(1,24,feature_num)[0][23][3]=X_[i].reshape(1,24,feature_num)[0][23][2]
#     X_[i+1].reshape(1,24,feature_num)[0][23][4]=X_[i].reshape(1,24,feature_num)[0][23][3]
#     icsr2 = test_x[i+1][0][1]
    
#     ss2 =test_x[i+1][0][1]
#     ss2 = test_x[i+1][0][1]
#     hour2 = test_x[i+1][0][1]
#     tca2 = test_x[i+1][0][2]
#     ta2 = test_x[i+1][0][3]
#     hm2 = test_x[i+1][0][4]
#     if hour2==0:
#         pre=0
#     hm2 = test_x[i+1][0][3]
#     hm2 =test_x[i+1][0][5]
#     ss2 += [srr]
    pred += [pre]
#    watts2+=[pre][0][2]
#    watts3+=[pre]
#    temp =test_x[i+1][0][3]
#     vss =test_x[i+1][0][3]
#     rnn = test_x[i+1][0][2]
# #     vss =test_x[i+1][0][3]
#     X_ = np.array(np.append(X_[0][1:],[pre,icsr2]).reshape(1,24,2),dtype=np.float32)
#     print(pred)
#     X_ = np.array(np.append(X_[0][1:],[pre, time,icsr2,ts2]).reshape(1,24,4),dtype=np.float32)


# In[119]:


pred=pred[-2:]


# In[125]:


pred+=[0]


# In[126]:


len(pred)


# In[332]:


t=pd.DataFrame({"date":aaaa.date,"pred_watts":pred,"hour":np.tile(range(0,24),(int(len(X_)/24),)),
                    "num":range(0,24*(int(len(X_)/24))),"real_watts":aaaa.watts[24:]})


# In[333]:


t=pd.DataFrame({"date":aaaa.date[24:],"pred_watts":model.predict(X_).reshape(len(X_,)),"hour":np.tile(range(0,24),(int(len(X_)/24),)),
                    "num":range(0,24*(int(len(X_)/24))),"real_watts":aaaa.watts[24:]})


# In[334]:


nemo1=t.loc[:,["date","hour","pred_watts","real_watts"]].reset_index(drop=True)


# In[335]:


nemo1.to_csv("./nemo1_eval_new.csv",sep=",")


# In[ ]:


method=["nn"]
eval_df = pd.DataFrame({})
t["APE"] = ((abs(t.real-t.pred))/abs(t.real))*100
t["SE"] = ((t.real-t.pred)**2)
t["SE"][t.APE==np.inf]=np.nan
MAPE = [sum(t.APE[t.APE!=np.inf])/(len(t.APE[t.APE!=np.inf]))]
RMSE = np.sqrt()
eval_df["method"]=method
eval_df["MAPE"]=MAPE
eval_df["RMSE"]=np.sqrt(RMSE)
eval_df


# In[100]:


mean_absolute_error(nemo1.real_watts,nemo1.pred_watts)*100


# In[101]:


np.sqrt(mean_squared_error(nemo1.real_watts,nemo1.pred_watts))


# In[77]:


ax=t[360:480].plot(x="num",y="pred",legend=False)
t[360:480].plot(x="num",y="real",color="r",ax=ax,legend=False)
ax.yaxis.grid()
ax.xaxis.grid()
ax.figure.legend()
plt.title("PV prediction")
plt.show()


# In[146]:


t=pd.DataFrame({"date":aaaa.date[24:],"pred":model.predict(X_).reshape(len(X_,)),"hour":np.tile(range(0,24),(int(len(X_)/24),)),
                    "num":range(0,24*(int(len(X_)/24))),"real":aaaa.watts[24:]})


# In[85]:


method=["nn"]
eval_df = pd.DataFrame({})
t["APE"] = ((abs(t.real-t.pred))/abs(t.real))*100
t["SE"] = ((t.real-t.pred)**2)
t["SE"][t.APE==np.inf]=np.nan
MAPE = [sum(t.APE[t.APE!=np.inf])/(len(t.APE[t.APE!=np.inf]))]
RMSE = [sum(t.SE[t.SE>=0])/len(t.SE[t.SE>=0])]
eval_df["method"]=method
eval_df["MAPE"]=MAPE
eval_df["RMSE"]=np.sqrt(RMSE)
eval_df


# In[ ]:





# In[257]:


# 얼리스타핑
epochs = 10
batch_size = 24
#early_stopping = EarlyStopping(monitor='val_loss',patience=10, verbose=1)
model.fit(X, Y,
         batch_size=batch_size,
         epochs=epochs)


# In[101]:


t=pd.DataFrame({"pred":model.predict(test_x).reshape(len(test_x,)),"real":test_y.reshape(len(test_y),),"hour":np.tile(range(0,24),(int(len(test_y)/24),)),"num":range(0,24*(int(len(test_y)/24)))})
t["APE"] = ((abs(t.real-t.pred))/abs(t.real))*100
t[:48],t[48:96],t[96:144],t[144:192]


# In[102]:


ax=t[:].plot(x="num",y="pred",legend=False)
ax2 = ax.twinx()
t[:].plot(x="num",y="real",color="r",ax=ax2,legend=False)
ax.yaxis.grid()
ax.xaxis.grid()
ax.figure.legend()
plt.title("PV prediction")
# plt.savefig("./pv2_nemo1.png",dpi=400)


# In[250]:


method=["nn"]
eval_df = pd.DataFrame({})
t["APE"] = ((abs(t.real-t.pred))/abs(t.real))*100
t["SE"] = ((t.real-t.pred)**2)
t["SE"][t.APE==np.inf]=np.nan
MAPE = [sum(t.APE[t.APE!=np.inf])/(len(t.APE[t.APE!=np.inf]))]
RMSE = [sum(t.SE[t.SE>=0])/len(t.SE[t.SE>=0])]
eval_df["method"]=method
eval_df["MAPE"]=MAPE
eval_df["RMSE"]=np.sqrt(RMSE)
eval_df


# In[239]:





# In[240]:





# In[241]:





# In[ ]:





# In[258]:


t=pd.DataFrame({"date":aaaa.date[24:],"pred":model.predict(X_).reshape(len(X_,)),"hour":np.tile(range(0,24),(int(len(X_)/24),)),"num":range(0,24*(int(len(X_)/24)))})
# t["APE"] = ((abs(t.real-t.pred))/abs(t.real))*100
t2=t[-48:]
aaa = pd.read_csv("./nemo_real.csv")
t2["real"]=aaa.real.values


# In[259]:


ax=t2[:].plot(x="num",y="pred",legend=False)
ax2 = ax.twinx()
t2[:].plot(x="num",y="real",color="r",ax=ax2,legend=False)
ax.yaxis.grid()
ax.xaxis.grid()
ax.figure.legend()
plt.title("PV prediction")
plt.savefig("./pv2_nemo1.png",dpi=400)


# In[260]:


method=["nn"]
eval_df = pd.DataFrame({})
t2["APE"] = ((abs(t2.real-t2.pred))/abs(t2.real))*100
t2["SE"] = ((t2.real-t2.pred)**2)
t2["SE"][t2.APE==np.inf]=np.nan
MAPE = [sum(t2.APE[t2.APE!=np.inf])/(len(t2.APE[t2.APE!=np.inf]))]
RMSE = [sum(t2.SE[t2.SE>=0])/len(t2.SE[t2.SE>=0])]
eval_df["method"]=method
eval_df["MAPE"]=MAPE
eval_df["RMSE"]=np.sqrt(RMSE)
eval_df


# In[192]:


model.save_weights("./pv_20200204_m2_1.h5")


# # DNN

# In[100]:


X = df_1.loc[:,model2]
Y = df_1.watts.values
aaaa=dong_test.copy().reset_index(drop=True)
X_ = aaaa.loc[:,model2]
Y_ = aaaa.watts.values


# In[102]:


###### DNN
model = Sequential()
model.add(Dense(1000,kernel_initializer=tf.initializers.glorot_normal,input_shape=[len(X.keys())],activation='elu'))
model.add(Dense(1000,kernel_initializer=tf.initializers.glorot_normal,activation='elu'))
model.add(Dense(1000,kernel_initializer=tf.initializers.glorot_normal,activation='elu'))
model.add(Dense(1000,kernel_initializer=tf.initializers.glorot_normal,activation='elu'))
model.add(Dense(1000,kernel_initializer=tf.initializers.glorot_normal,activation='elu'))
model.add(Dense(1))

## 옵티마이져
#optimizer = Adam(lr=0.0001, beta_1 = 0.9, beta_2 = 0.999)    # ADAM 옵티마이져, loss Mse
# optimizer = RMSprop(learning_rate=0.001, rho= 0.9, momentum = 0.0, epsilon=0.0000001)    # ADAM 옵티마이져, loss Mse
#optimizer = SGD(learning_rate=0.0001,momentum=0.9,nesterov=True) #SGD는 별로 도움이 안됨
optimizer = Adamax(lr=0.001, beta_1 = 0.9, beta_2 = 0.999)
#optimizer = Adagrad(lr=0.001, epsilon=1e-6) X
#optimizer = Adadelta(lr=1.0, rho=0.95, epsilon=None, decay=0.0)
#@optimizer = MomentumOptimizer(learning_rate=0.001, momentum=0.9) 못씀
model.compile(loss = 'mean_squared_error',
             optimizer=optimizer)


# In[119]:


epochs = 200
batch_size = 24
early_stopping = EarlyStopping(monitor='val_loss',patience=10, verbose=1)
model.fit(train_x, train_y,
         batch_size=batch_size,
         epochs=epochs,
         validation_data=(val_x,val_y),
         callbacks=[early_stopping])


# In[108]:


batch_size = 30
mae=[]
epochs = 1
for i in range(0,100):
    print(i+1)

    model.fit(X, Y,
         batch_size=batch_size,
         epochs=epochs)
    t=pd.DataFrame({"date":aaaa.date[24:],"pred":model.predict(X_[24:]).reshape(len(X_[24:],)),"hour":np.tile(range(0,24),(int(len(X_[24:])/24),)),
                    "num":range(0,24*(int(len(X_[24:])/24))),"real":aaaa.watts[24:]})
# t["APE"] = ((abs(t.real-t.pred))/abs(t.real))*100
    print(len(aaaa.watts.values[24:]))
    ax=t[480:720].plot(x="num",y="pred",legend=False)
    t[480:720].plot(x="num",y="real",color="r",ax=ax,legend=False)
    ax.yaxis.grid()
    ax.xaxis.grid()
    ax.figure.legend()
    plt.title("PV prediction")
    plt.show()
    plt.close()
    method=["nn"]
    eval_df = pd.DataFrame({})
    t["AE"] = abs(t.real-t.pred)
    t["SE"] = (t.real-t.pred)**2
    MAE = [sum(t.AE)/(len(t.AE))]
    MSE = [sum(t.SE)/len(t.SE)]
    eval_df["method"]=method
    eval_df["MAE"]=MAE
    eval_df["MSE"]=MSE
    print(eval_df)
    mae += [eval_df.MAE.values[0]]
    if ((eval_df.MAE.values[0]<=np.min(mae))):
        model.save_weights("pv_20200210_bandi_m2_"+str(i)+".h5")
#plt.savefig("./pv2_nemo1.png",dpi=400)


# In[139]:


len(test_x[:840])/24


# In[135]:





# In[140]:


t=pd.DataFrame({"pred":model.predict(test_x).reshape(len(test_x,)),"real":test_y.reshape(len(test_y))})
t["APE"] = ((abs(t.real-t.pred))/abs(t.real))*100


# In[145]:


plt.plot(model.predict(aaaa.loc[:,model2]).reshape(len(aaaa.loc[:,model2]),))


# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:




