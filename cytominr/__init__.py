def aggregate(population, variables, strata, operation="mean", **kwargs):
    """

    :param population:
    :param variables:
    :param strata:
    :param operation:
    :param kwargs:
    """

    groups = population.groupby(strata)

    if operation is "mean":
        groups[variables].mean()
    elif operation is "median":
        groups[variables].median()
    else:
        pass


def normalize(population, variables, strata, sample, operation="standardize", **kwargs):
    """

    :param population:
    :param variables: 
    :param strata: 
    :param sample: 
    :param operation: 
    :param kwargs: 
    """
    if operation is "robustize":
        pass
    elif operation is "standardize":
        pass
    else:
        pass


def select(population, variables, operation="variance_threshold", **kwargs):
    """

    :param population:
    :param variables:
    :param operation:
    :param kwargs:
    """
    if operation is "variance_threshold":
        pass
    elif operation is "correlation_threshold":
        pass
    elif operation is "drop_na_columns":
        pass
    else:
        pass


def transform(population, variables, operation="generalized_log", **kwargs):
    """

    :param population:
    :param variables:
    :param operation:
    :param kwargs:
    """
    if operation is "generalized_log":
        pass
    else:
        pass
