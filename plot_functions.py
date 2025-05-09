# plotting functions for utility

import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse
import geopandas as gpd
import pandas as pd
from PIL import Image
import numpy as np


def plot_directional_ellipse_gdf(gdf, ax=None, n_std=2.0, **kwargs):
    """
    Plot a directional (covariance-based) ellipse from a GeoDataFrame of Points.
    
    Parameters:
    - gdf: GeoDataFrame with Point geometries
    - ax: matplotlib axis (optional)
    - n_std: Number of standard deviations (e.g., 1, 2, 3)
    - kwargs: passed to matplotlib.patches.Ellipse (e.g., edgecolor, facecolor)
    """
    if ax is None:
        fig, ax = plt.subplots()

    # Extract coordinates from geometry
    coords = np.array([[geom.x, geom.y] for geom in gdf.geometry if geom is not None])

    if len(coords) < 2:
        raise ValueError("Need at least two valid Point geometries to compute an ellipse.")

    # Compute mean and covariance
    mean = coords.mean(axis=0)
    cov = np.cov(coords, rowvar=False)

    # Eigen-decomposition of covariance matrix
    vals, vecs = np.linalg.eigh(cov)
    order = vals.argsort()[::-1]
    vals, vecs = vals[order], vecs[:, order]

    width, height = 2 * n_std * np.sqrt(vals)
    angle = np.degrees(np.arctan2(*vecs[:, 0][::-1]))

    ellipse = Ellipse(xy=mean, width=width, height=height, angle=angle, **kwargs)
    ax.add_patch(ellipse)

    # Plot the points and ellipse
    gdf.plot(ax=ax, color='black', markersize=5)
    ax.set_aspect('equal')
    return ax


def plot_directional_ellipse_gdf(gdf, ax=None, n_std=2.0, **kwargs):
    """
    Plot a directional (covariance-based) ellipse from a GeoDataFrame of Points.
    
    Parameters:
    - gdf: GeoDataFrame with Point geometries
    - ax: matplotlib axis (optional)
    - n_std: Number of standard deviations (e.g., 1, 2, 3)
    - kwargs: passed to matplotlib.patches.Ellipse (e.g., edgecolor, facecolor)
    """
    if ax is None:
        fig, ax = plt.subplots()

    # Extract coordinates from geometry
    coords = np.array([[geom.x, geom.y] for geom in gdf.geometry if geom is not None])

    if len(coords) < 2:
        raise ValueError("Need at least two valid Point geometries to compute an ellipse.")

    # Compute mean and covariance
    mean = coords.mean(axis=0)
    cov = np.cov(coords, rowvar=False)

    # Eigen-decomposition of covariance matrix
    vals, vecs = np.linalg.eigh(cov)
    order = vals.argsort()[::-1]
    vals, vecs = vals[order], vecs[:, order]

    width, height = 2 * n_std * np.sqrt(vals)
    angle = np.degrees(np.arctan2(*vecs[:, 0][::-1]))

    ellipse = Ellipse(xy=mean, width=width, height=height, angle=angle, **kwargs)
    ax.add_patch(ellipse)

    # Plot the points and ellipse
    gdf.plot(ax=ax, color='black', markersize=5)
    ax.set_aspect('equal')
    return ax