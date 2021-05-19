# Training of the best models for the PD data
import logging
import numpy as np
import pandas as pd
import sklearn.ensemble as sk
import shap
from sklearn.preprocessing import OrdinalEncoder

logging.basicConfig(format='%(asctime)s - %(message)s', level=logging.INFO, filename="06_out.log")
logging.info("libraries imported!")

train = pd.read_csv("data/pd_train.csv", low_memory=False)
train = train.dropna(axis=0)
test = pd.read_csv("data/pd_test.csv", low_memory=False)
test = test.dropna(axis=0)
test = test.sample(n = 50000)
logging.info("train and test data read in")

#### Frequency Model ----

X = train.drop(columns=["ULTIMATE_CLAIM_COUNT", "severity", "log_severity", "ULTIMATE_AMOUNT", "form", "X_VAR46", "X_VAR2", "X_VAR19", "X_VAR34", "EARNED_EXPOSURE"])
y1 = train[["ULTIMATE_CLAIM_COUNT"]].values.squeeze()
wt = train[["EARNED_EXPOSURE"]].values.squeeze()
logging.info("train data split into predictors and response for freq mod")

logging.info("creating encoding")
enc = OrdinalEncoder(handle_unknown = "use_encoded_value", unknown_value = -1)
logging.info("fitting encoding")
enc.fit(X)
logging.info("encoding the training data")
X = enc.transform(X)
logging.info("training data encoding created")

logging.info("setting tuning parameters for model")
freq_mod = sk.RandomForestClassifier(n_estimators = 100, max_depth = 20, max_features = 20, min_impurity_decrease = .001, max_samples = 0.632, n_jobs = -1)
logging.info("fitting frequency model")
freq_mod.fit(X = X, y = y1, sample_weight = wt)
logging.info("frequency model fit")

logging.info("dropping columns in the test data")
X_test = test.drop(columns=["ULTIMATE_CLAIM_COUNT", "severity", "log_severity", "ULTIMATE_AMOUNT", "form", "X_VAR46", "X_VAR2", "X_VAR19", "X_VAR34", "EARNED_EXPOSURE"])
logging.info("saving the test data")
X_test.to_csv("output/pd_final_mod_test_data.csv")
logging.info("encoding the test data")
X_test = enc.transform(X_test)
logging.info("predicting on the test data")
freq_preds = freq_mod.predict_proba(X_test)
logging.info("predictions made for frequency on the test data")

logging.info("coercing predictions to data frame")
freq_preds = pd.DataFrame(freq_preds, columns = ["p0", "p1", "p2", "p3"])

logging.info("saving predictions on the test data")
freq_preds.to_csv("output/pd_freq_preds_final_mod.csv")
logging.info("predictions saved")

freq_ex = shap.Explainer(freq_mod)
logging.info("the next line is the expected value")
logging.info(freq_ex.expected_value)
freq_shap = freq_ex.shap_values(X_test)
logging.info("shap values calculated")

p0_shap = pd.DataFrame(freq_shap[0])
p1_shap = pd.DataFrame(freq_shap[1])
p2_shap = pd.DataFrame(freq_shap[2])
p3_shap = pd.DataFrame(freq_shap[3])

p0_shap.to_csv("output/pd_freq_shap_values_final_mod_0.csv")
p1_shap.to_csv("output/pd_freq_shap_values_final_mod_1.csv")
p2_shap.to_csv("output/pd_freq_shap_values_final_mod_2.csv")
p3_shap.to_csv("output/pd_freq_shap_values_final_mod_3.csv")

#### Severity Model ----

logging.info("filtering training data for the severity model")
X2 = train.loc[train["severity"] > 1,:]
X2 = X2.dropna(axis=0)
X2 = X2.drop_duplicates()

logging.info("finalizing training data and the response for the severity model")
X3 = X2.drop(columns=["ULTIMATE_CLAIM_COUNT", "severity", "log_severity", "ULTIMATE_AMOUNT", "form", "X_VAR46", "X_VAR2", "X_VAR19", "X_VAR34", "EARNED_EXPOSURE"])
y2 = X2[["severity"]].values.squeeze()
y2 = np.log(y2)

logging.info("encoding the training data for the severity model")
X3 = enc.transform(X3)

logging.info("creating severity model")
sev_mod = sk.RandomForestRegressor(n_estimators=200, max_depth=30, max_features=20,min_impurity_decrease=.0001, max_samples=0.632, n_jobs=-1)
logging.info("fitting severity model")
sev_mod.fit(X = X3, y = y2)
logging.info("severity model fit")

logging.info("predicting on test set with severity model")
sev_preds = sev_mod.predict(X_test)
logging.info("predictions made")

logging.info("saving predictions")
sev_preds = pd.DataFrame(sev_preds)
sev_preds.to_csv("output/pd_sev_preds_final_mod.csv")
logging.info("severity predictions saved")

logging.info("explaining severity model predictions")
sev_ex = shap.Explainer(sev_mod)
logging.info("the next line in the expected value")
logging.info(sev_ex.expected_value)
sev_shap = sev_ex.shap_values(X_test, approximate=True)
logging.info("shap values calculated")

sev_shap = pd.DataFrame(sev_shap)
logging.info("saving severity shap values")
sev_shap.to_csv("output/pd_sev_shap_values_final_mod.csv")
logging.info("severity shap values saved")

logging.info("closing python")
exit()



