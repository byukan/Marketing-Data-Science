# coding: utf-8

from collections import Counter

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

transactions = pd.read_csv("purchases.txt", delimiter="\t", header=None)

transactions.columns = ['cust_id', 'purchase_amt', 'date']
transactions['year'] = transactions['date'].apply(lambda x: x[:4])
transactions['purchase_amt'] = transactions['purchase_amt'].apply(lambda x: float(x))

transactions.head()

fig, axs = plt.subplots(1, 1, figsize=(10, 5))

axs.hist(transactions['purchase_amt'], bins=50);
axs.set_title('Histogram of Purchase Amounts');

axs.set_xlabel("Purchase Amount (Dollars)");
axs.set_ylabel("Count");

transactions = transactions.groupby(['cust_id', 'year'])['purchase_amt'].sum().reset_index()
purchase_amts = transactions['purchase_amt']
purchase_amts.mean()

boolean_matrix = pd.crosstab(transactions.cust_id, [transactions.year], rownames=['cust_id'], colnames=['year'])
boolean_matrix = boolean_matrix.applymap(lambda x: 1 if x > 0 else 0)

boolean_matrix_restricted = boolean_matrix[boolean_matrix['2005'] == 1]
boolean_matrix_restricted.head()


# some useful functions

def build_recency_and_RFM_matrix(boolean_matrix, transactions):
    """
    INPUT: boolean matrix with users as rows, years as columns.
    cell value 1 if user made a purchase in that year.
    cell value 0 if user made no purchase in that year.
    OUTPUT:
    recency matrix: matrix showing user's recency score for a given year.
    RFM matrix: matrix showing user's RFM tuple for a given year
    """
    recency_matrix = []
    RFM_matrix = []

    for row_num, boolean_row in enumerate(boolean_matrix.values):
        cust_id = int(boolean_matrix.index[row_num])

        frequency = 0
        recency = 0

        recency_row = []
        RFM_row = []

        for col_num, cell_value in enumerate(boolean_row):
            year = boolean_matrix.columns[col_num]

            if cell_value == 1:
                transactions_by_cust = transactions[transactions['cust_id'] == cust_id]
                transactions_by_cust_by_year = transactions_by_cust[transactions_by_cust['year'] == year]

                monetary = float(transactions_by_cust_by_year['purchase_amt'])

                frequency += 1
                recency = 0
            else:
                recency += 1

            recency_row.append(recency)
            RFM_row.append((recency, frequency, monetary))

        recency_matrix.append(recency_row)
        RFM_matrix.append(RFM_row)

    return recency_matrix, RFM_matrix


def convert_to_discrete_tuple(continuous_tuple):
    """
    INPUT: continuous RFM tuple corresponding to a user
    OUTPUT: discrete tuple according to the segementation we defined
    """

    if continuous_tuple[0] <= 0:
        R = 1
    elif continuous_tuple[0] < 6:
        R = 2
    else:
        R = 3
    if continuous_tuple[1] > 5:
        F = 1
    else:
        F = 2
    if continuous_tuple[2] > 50:
        M = 1
    elif continuous_tuple[2] > 30 or continuous_tuple[2] == 50:
        M = 2
    else:
        M = 3
    return R, F, M


def convert_to_state_matrix(continuous_tuple_matrix, RFM_states):
    """
    INPUT:
    matrix of RFM tuples for each user/year
    dictionary mapping RFM tuples to state numbers

    OUTPUT:
    matrix of states for each user/year
    """

    new_matrix = []

    for row in continuous_tuple_matrix:
        new_row = []

        for continuous_tuple in row:
            discrete_tuple = convert_to_discrete_tuple(continuous_tuple)

            state = RFM_states[discrete_tuple]

            new_row.append(state)

        new_matrix.append(new_row)

    return new_matrix


# dictionary mapping RFM tuples to numbered states
RFM_states = {(1, 1, 1): 1,
              (1, 1, 2): 2,
              (1, 1, 3): 3,
              (1, 2, 1): 4,
              (1, 2, 2): 5,
              (1, 2, 3): 6,
              (2, 1, 1): 7,
              (2, 1, 2): 8,
              (2, 1, 3): 9,
              (2, 2, 1): 10,
              (2, 2, 2): 11,
              (2, 2, 3): 12,
              (3, 1, 1): 13,
              (3, 1, 2): 14,
              (3, 1, 3): 15,
              (3, 2, 1): 16,
              (3, 2, 2): 17,
              (3, 2, 3): 18}

recency_matrix, RFM_matrix = build_recency_and_RFM_matrix(boolean_matrix_restricted, transactions)

state_matrix = convert_to_state_matrix(RFM_matrix, RFM_states)

frequencies = []

for row in RFM_matrix:
    for cell_value in row:
        frequencies.append(cell_value[1])

recency_df = pd.DataFrame(np.array(recency_matrix))

recency_df.index = boolean_matrix_restricted.index
recency_df.columns = boolean_matrix_restricted.columns

recency_df.head()

total_at_recency_dict = {}
total_transition_recencies_dict = {}

state_df = pd.DataFrame(np.array(state_matrix))

state_df.columns = boolean_matrix_restricted.columns
state_df.index = boolean_matrix_restricted.index

state_df.head()

# build transition matrix for recency
for recency_score in range(11):
    total_at_recency = 0

    total_transition_states_observed = Counter()

    for year in state_df.columns:
        # no data for 2016 so leave 2015 out
        if year == '2015':
            break

        # reduce dataframe to people who were in this state this year
        this_recency_this_year = recency_df[recency_df[year] == recency_score]

        # total number of people observed in this state this year
        total_this_recency_this_year = this_recency_this_year.shape[0]
        total_at_recency += total_this_recency_this_year

        # get count of states observed the next year
        next_year = str(int(year) + 1)

        observed_recencies_next_year = Counter(list(this_recency_this_year[next_year]))

        total_transition_states_observed += observed_recencies_next_year

    total_at_recency_dict[recency_score] = total_at_recency
    total_transition_recencies_dict[recency_score] = total_transition_states_observed

# In[70]:


# build recency transition matrix
recency_transition_matrix = np.zeros((11, 11))

for i in range(10):
    for j in range(11):
        transition_prob = total_transition_recencies_dict[i][j] / total_at_recency_dict[i]

        recency_transition_matrix[i, j] = transition_prob

# In[71]:


pd.DataFrame(recency_transition_matrix)

# In[72]:


frequencies = []

for row in RFM_matrix:
    for cell_value in row:
        frequencies.append(cell_value[1])

# In[74]:


# dictionary that maps states to the total customers observed in that state
# key: state number
# value: integer giving total number
total_in_state_dict = {}

# dictionary that maps states to the distribution of states the year following
# key: state number
# value: counter of states the following year
total_transition_states_dict = {}

# In[75]:


# build transition matrix for RFMs
for state in range(1, 19):
    total_in_state = 0

    total_transition_states_observed = Counter()

    for year in state_df.columns:
        # no data for 2016 so leave 2015 out
        if year == '2015':
            break

        # reduce dataframe to people who were in this state this year
        this_state_this_year = state_df[state_df[year] == state]

        # total number of people observed in this state this year
        total_this_state_this_year = this_state_this_year.shape[0]
        total_in_state += total_this_state_this_year

        # get count of states observed the next year
        next_year = str(int(year) + 1)

        observed_states_next_year = Counter(list(this_state_this_year[next_year]))

        total_transition_states_observed += observed_states_next_year

    total_in_state_dict[state] = total_in_state
    total_transition_states_dict[state] = total_transition_states_observed

total_in_state_dict

# In[76]:


total_transition_states_dict

# In[77]:


# build transition matrix
RFM_transition_matrix = np.zeros((18, 18))

for i in range(1, 18):
    for j in range(1, 18):
        # absorbtion states
        if i >= 13 and i == j:
            transition_prob = 1
        elif i >= 13 and i != j:
            transition_prob = 0
        else:
            transition_prob = total_transition_states_dict[i][j] / total_in_state_dict[i]

        RFM_transition_matrix[i - 1, j - 1] = transition_prob

# In[78]:


pd.DataFrame(RFM_transition_matrix)

# In[79]:


# build state matrices for all customers that started after 2005

years = ['2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014']

boolean_matrix_test = boolean_matrix.copy()

state_dataframes = []

for year in years:
    no_purchase_first_year = boolean_matrix_test[boolean_matrix_test[year] == 0]
    first_purchase_next_year = no_purchase_first_year[no_purchase_first_year[str(int(year) + 1)] == 1]
    first_purchase_next_year.drop(year, axis=1, inplace=True)

    recency_matrix_here, RFM_matrix_here = build_recency_and_RFM_matrix(first_purchase_next_year, transactions)
    state_matrix_here = convert_to_state_matrix(RFM_matrix_here, RFM_states)

    state_df_here = pd.DataFrame(state_matrix_here)
    state_df_here.columns = first_purchase_next_year.columns
    state_df_here.index = first_purchase_next_year.index

    state_dataframes.append(state_df_here)

    boolean_matrix_test.drop(year, axis=1, inplace=True)

# In[80]:


initial_counter = Counter(state_df['2015'])

for individual_state_df in state_dataframes:
    individual_counter = Counter(individual_state_df['2015'])

    initial_counter += individual_counter

# In[81]:


# build starting state vector

starting_state = []

for i in range(1, 19):
    if i in initial_counter:
        starting_state.append(starting_state_counts[i])

    else:
        starting_state.append(0)

starting_state = np.array(starting_state)

# In[ ]:


starting_state

# In[ ]:


new_customer_state_counts = Counter(state_df['2005'])

total_new_customers = [state_df['2005'].count()]

for individual_state_df in state_dataframes:
    new_customer_state_counts += Counter(individual_state_df[individual_state_df.columns[0]])

    total_new_customers.append(individual_state_df.shape[0])

average_new_customers = np.mean(total_new_customers) // 1

new_customer_state_dist = {}

for state, state_count in new_customer_state_counts.items():
    new_customer_state_dist[state] = state_count / np.sum(total_new_customers)

new_customer_state_dist

new_state_vector_each_year = np.zeros(18)
new_state_vector_each_year[3] = average_new_customers * new_customer_state_dist[4] // 1
new_state_vector_each_year[4] = average_new_customers * new_customer_state_dist[5] // 1
new_state_vector_each_year[5] = average_new_customers * new_customer_state_dist[6] // 1

new_state_vector_each_year

# In[ ]:


# build the reward vector

avg_purchase_amt = purchase_amts.mean()

reward_vector = np.zeros(18)

for i in range(6):
    reward_vector[i] = avg_purchase_amt - 25

for i in range(6, 12):
    reward_vector[i] = - 25

for i in range(12, 18):
    reward_vector[i] = 0

# In[ ]:


I_18 = np.identity(18)

CLV = (np.linalg.inv(I_18 - (1 + d) ** (-1) * RFM_transition_matrix)).dot(reward_vector)

CLV.reshape(len(CLV), 1)

# In[ ]:


# without policy

revenue_no_policy = []

total_reward = reward_vector * (((1 + d) ** -1) * RFM_transition_matrix).dot(
    starting_state) + new_state_vector_each_year
new_state = starting_state.copy()

revenue_no_policy.append(np.sum(total_reward))

for i in range(9):
    new_state = (((1 + d) ** -1) * RFM_transition_matrix).dot(new_state) + new_state_vector_each_year
    reward_this_year = reward_vector * new_state

    revenue_no_policy.append(np.sum(reward_this_year))

    total_reward += reward_this_year

revenue_no_policy

np.sum(revenue_no_policy)

for year, rev in enumerate(revenue_no_policy, 2016):
    print(str(year) + "\t" + str(rev))

# with policy

revenue_with_policy = []

reward_vector_with_policy = reward_vector.copy()

# set all the negative CLVs to zero
reward_vector_with_policy[8:] = 0

total_reward = reward_vector_with_policy * (((1 + d) ** -1) * RFM_transition_matrix).dot(
    starting_state) + new_state_vector_each_year
new_state = starting_state.copy()

revenue_with_policy.append(np.sum(total_reward))

for i in range(9):
    new_state = (((1 + d) ** -1) * RFM_transition_matrix).dot(new_state) + new_state_vector_each_year
    reward_this_year = reward_vector_with_policy * new_state

    revenue_with_policy.append(np.sum(reward_this_year))

    total_reward += reward_this_year

money = []

for year, rev in enumerate(revenue_with_policy, 2016):
    print(str(year) + "\t" + str(rev))
    money.append(str(year) + "\t" + str(rev))

fig, axs = plt.subplots(1, 1, figsize=(10, 5))

total_purchases = boolean_matrix_restricted.apply(lambda x: sum(x), axis=1)

axs.hist(total_purchases, bins=11)

axs.set_title("Histogram of Total Purchases")
axs.set_xlabel("Total Purchases Made")
axs.set_ylabel("Count")

np.sum(revenue_with_policy)

reward_vector.reshape(18, 1)

CLV.reshape(18, 1)

np.sum(new_state_vector_each_year)
