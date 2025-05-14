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


def create_gif(file_names, gif_name, frame_duration=200):
    """
        Creates a GIF from images in a folder.
    
        Parameters:
        file_names: list of image paths
        gif_name (str): path of the output GIF file (e.g., "animation.gif").
        frame_duration (int): Duration of each frame in milliseconds.
    """
    images = []
    
        # Sort files to ensure correct order
    file_names.sort()
        
    for filename in file_names:
        if filename.lower().endswith(('.png', '.jpg', '.jpeg')):
            try:
                img = Image.open(filename)
                images.append(img)
            except Exception as e:
                print(f"Error opening image {filename}: {e}")
    
    if images:
        images[0].save(gif_name, save_all=True, append_images=images[1:], 
                          duration=frame_duration, loop=0)
        print(f"GIF saved as {gif_name} in {gif_name}")
    else:
        print("No valid images found in the specified folder.")